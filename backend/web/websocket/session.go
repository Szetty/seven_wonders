package websocket

import (
	"fmt"
	"github.com/Szetty/seven_wonders/backend/domain"
	"github.com/Szetty/seven_wonders/backend/logger"
	"github.com/google/uuid"
	"github.com/gorilla/websocket"
	"github.com/pkg/errors"
	"strings"
	"sync"
	"time"
)

const (
	// Time allowed to write a message to the peer.
	writeWait = 10 * time.Second

	// Time allowed to read the next pong message from the peer.
	pongWait = 60 * time.Second

	// Send pings to peer with this period. Must be less than pongWait.
	pingPeriod = (pongWait * 9) / 10

	// Maximum message size allowed from peer.
	maxMessageSize = 512

	// Tries to send queue message from client with this period.
	hubRetryPeriod = 5 * time.Second

	// Time which is used to wait before sending offline message (user may reconnect)
	offlineWait = 5 * time.Second
)

// events
type (
	welcome struct{}
)

type RegisterUpstreamChannels struct {
	HubCh   chan<- domain.OriginEnvelope
	UsersCh chan<- domain.OriginEnvelope
}

type Session struct {
	ID       string
	ClientCh chan<- domain.OriginEnvelope
	EventCh  chan<- interface{}
	internalSession
}

type internalSession struct {
	clientCh  <-chan domain.OriginEnvelope
	eventCh   <-chan interface{}
	hubCh     chan<- domain.OriginEnvelope
	usersCh   chan<- domain.OriginEnvelope
	conn      *websocket.Conn
	hubQ      []domain.Envelope
	clientQ   []domain.Envelope
	offlineCh chan<- bool
	mtx       sync.Mutex
}

func newSession(username string, conn *websocket.Conn) *Session {
	id := username + "@" + uuid.New().String()
	clientCh := make(chan domain.OriginEnvelope, 100)
	eventCh := make(chan interface{})
	return &Session{
		ID:       id,
		ClientCh: clientCh,
		EventCh:  eventCh,
		internalSession: internalSession{
			clientCh: clientCh,
			eventCh:  eventCh,
			conn:     conn,
			hubQ:     []domain.Envelope{},
			clientQ:  []domain.Envelope{},
			mtx:      sync.Mutex{},
		},
	}
}

func (s *Session) refreshSession(conn *websocket.Conn) bool {
	//if s.conn != nil {
	//	s.closeWebsocket(s.conn, "Refreshing session")
	//}
	defer func() {
		clientCh := make(chan domain.OriginEnvelope, 100)
		s.conn = conn
		s.hubCh = nil
		s.usersCh = nil
		s.ClientCh = clientCh
		s.clientCh = clientCh
	}()
	wasOnline := false
	s.mtx.Lock()
	if s.offlineCh != nil {
		s.offlineCh <- true
		wasOnline = true
	}
	s.mtx.Unlock()
	return wasOnline
}

func (s *Session) startSession() {
	s.conn.SetReadLimit(maxMessageSize)
	s.conn.SetPongHandler(func(string) error { _ = s.conn.SetReadDeadline(time.Now().Add(pongWait)); return nil })
	go s.sessionRoutine()
	s.EventCh <- welcome{}
}

func (s *Session) sessionRoutine() {
	conn := s.conn
	logger.L.Infof(s.messageWithPrefix("Starting session %s"), s.ID)
	clientReaderCh := make(chan []domain.Envelope)
	hubTicker := time.NewTicker(hubRetryPeriod)
	pingTicker := time.NewTicker(pingPeriod)
	defer func() { hubTicker.Stop(); pingTicker.Stop() }()
	go s.clientReader(conn, clientReaderCh)
	for {
		select {
		case envelopes, ok := <-clientReaderCh:
			if !ok {
				s.sendDisconnected()
				s.setupOfflineNotifier()
				return
			}
			if len(envelopes) <= 0 {
				continue
			}
			s.processEnvelopes(envelopes)
		case <-hubTicker.C:
			if len(s.hubQ) > 0 {
				s.handleHubQ()
			}
		case e := <-s.eventCh:
			err := s.handleEvent(e, conn)
			if err != nil {
				return
			}
		case originEnvelope, ok := <-s.clientCh:
			if !ok {
				s.clientChannelClosed(conn)
				s.closeWebsocket(conn, "to client channel was closed")
				return
			}
			logger.L.Infof(s.messageWithPrefix("RECEIVED FROM CRUX: %#v"), originEnvelope)
			err := s.receiveFromCrux(conn, originEnvelope)
			if err != nil {
				s.sendDisconnected()
				s.setupOfflineNotifier()
				return
			}
		case <-pingTicker.C:
			err := s.pingClient(conn)
			if err != nil {
				return
			}
		}
	}
}

func (s *Session) clientReader(conn *websocket.Conn, clientReaderCh chan<- []domain.Envelope) {
	for {
		logger.L.Infof(s.messageWithPrefix("Waiting to receive"))
		err, envelopes := ReceiveEnvelopes(conn)
		if err != nil {
			if websocket.IsCloseError(errors.Cause(err), websocket.CloseNormalClosure) {
				logger.L.Infof(s.messageWithPrefix("WS was closed for session"))
				close(clientReaderCh)
				return
			}
			logger.L.Warnf(s.messageWithPrefix("Fail to read WS message: %v"), err)
			if websocket.IsUnexpectedCloseError(errors.Cause(err), websocket.CloseNormalClosure) {
				close(clientReaderCh)
				return
			}
			clientReaderCh <- []domain.Envelope{}
		}
		logger.L.Infof(s.messageWithPrefix("Received envelopes: %#v"), envelopes)
		clientReaderCh <- envelopes
	}
}

func (s *Session) sendDisconnected() {
	disconnectedMessage := domain.MessageBuilder{}.MessageType(domain.Disconnected).Build()
	s.hubCh <- domain.EnveloperBuilder{}.Data(disconnectedMessage).Build().WithOrigin(s.origin())
}

func (s *Session) setupOfflineNotifier() {
	logger.L.Infof(s.messageWithPrefix("WS for session gone offline, setting up offline notifier"))
	offlineCh := make(chan bool)
	s.mtx.Lock()
	s.offlineCh = offlineCh
	s.mtx.Unlock()
	hubCh := s.hubCh
	ticker := time.NewTicker(offlineWait)
	select {
	case <-offlineCh:
		break
	case <-ticker.C:
		offlineMessage := domain.MessageBuilder{}.MessageType(domain.GotOffline).Build()
		hubCh <- domain.EnveloperBuilder{}.Data(offlineMessage).Build().WithOrigin(s.origin())
	}
	s.mtx.Lock()
	close(s.offlineCh)
	s.offlineCh = nil
	s.mtx.Unlock()
}

func (s *Session) processEnvelopes(envelopes []domain.Envelope) {
	logger.L.Infof(s.messageWithPrefix("Processing envelopes: %#v"), envelopes)
	for _, envelope := range envelopes {
		switch m := envelope.Data.(type) {
		case domain.Message:
			s.handleMessage(envelope, m)
		default:
			//TODO implement ACKs and other envelope types
		}
	}

	if s.hubCh != nil {
		if len(s.hubQ) > 0 {
			logger.L.Infof(s.messageWithPrefix("Sending to hub from reader channel"))
			for _, envelope := range s.hubQ {
				s.hubCh <- envelope.WithOrigin(s.origin())
			}
			s.hubQ = []domain.Envelope{}
		}
	} else {
		logger.L.Infof(s.messageWithPrefix("No hub channel yet"))
	}
}

func (s *Session) handleMessage(envelope domain.Envelope, message domain.Message) {
	switch message.MessageType {
	case domain.OnlineUsers:
		s.usersCh <- envelope.WithOrigin(s.origin())
	default:
		s.hubQ = append(s.hubQ, envelope)
	}
}

func (s *Session) handleHubQ() {
	logger.L.Infof(s.messageWithPrefix("Hub queue is NOT empty, trying to send"))
	if s.hubCh != nil {
		logger.L.Infof(s.messageWithPrefix("Sending to hub from queue"))
		for _, envelope := range s.hubQ {
			s.hubCh <- envelope.WithOrigin(s.origin())
		}
		s.hubQ = []domain.Envelope{}
	}
}

func (s *Session) handleEvent(e interface{}, conn *websocket.Conn) error {
	logger.L.Infof(s.messageWithPrefix("EVENT: %#v"), e)
	switch event := e.(type) {
	case RegisterUpstreamChannels:
		s.hubCh = event.HubCh
		s.usersCh = event.UsersCh
	case welcome:
		envelope := domain.EnveloperBuilder{}.
			Data(
				domain.MessageBuilder{}.
					MessageType(domain.Welcome).
					Body("").
					Build(),
			).
			UUID(uuid.New().String()).
			Build()
		err := s.sendToWS(conn, []domain.Envelope{envelope})
		if err != nil {
			return err
		}
	}
	return nil
}

func (s *Session) receiveFromCrux(conn *websocket.Conn, originEnvelope domain.OriginEnvelope) error {
	_ = conn.SetWriteDeadline(time.Now().Add(writeWait))
	s.clientQ = append(s.clientQ, originEnvelope.Envelope)

	s.readAllMessagesFromChannel(conn)

	logger.L.Infof(s.messageWithPrefix("Sending envelopes: %#v"), s.clientQ)

	err := s.sendToWS(conn, s.clientQ)
	if err != nil {
		return err
	}
	s.clientQ = []domain.Envelope{}
	return nil
}

func (s *Session) readAllMessagesFromChannel(conn *websocket.Conn) {
	n := len(s.clientCh)
	logger.L.Infof("LENGTH of %#v is %d", s.clientCh, n)
	for i := 0; i < n; i++ {
		originEnvelope, ok := <-s.clientCh
		if !ok {
			s.clientChannelClosed(conn)
			return
		}
		s.clientQ = append(s.clientQ, originEnvelope.Envelope)
	}
}

func (s *Session) sendToWS(conn *websocket.Conn, envelopes []domain.Envelope) error {
	if err := conn.WriteJSON(envelopes); err != nil {
		if websocket.IsCloseError(err, websocket.CloseNormalClosure) {
			return err
		}
		if websocket.IsUnexpectedCloseError(err) {
			logger.L.Warnf(s.messageWithPrefix("Fail to write WS message: %v"), err)
			return err
		}
		reason := fmt.Sprintf("Could not write bundle message to WS: %v", err)
		s.closeWebsocket(conn, reason)
		return err
	}
	return nil
}

func (s *Session) clientChannelClosed(conn *websocket.Conn) {
	logger.L.Info(s.messageWithPrefix("Client channel was closed"))
	_ = conn.WriteMessage(websocket.CloseMessage, []byte{})
}

func (s *Session) pingClient(conn *websocket.Conn) error {
	_ = conn.SetWriteDeadline(time.Now().Add(writeWait))
	if err := conn.WriteMessage(websocket.PingMessage, nil); err != nil {
		if websocket.IsCloseError(err, websocket.CloseNormalClosure) {
			return err
		}
		if websocket.IsUnexpectedCloseError(err) {
			logger.L.Warnf(s.messageWithPrefix("Fail to send ping message: %v"), err)
			return err
		}
		reason := fmt.Sprintf("Could not write ping message to WS: %v", err)
		s.closeWebsocket(conn, reason)
		return err
	}
	return nil
}

func (s *Session) closeWebsocket(conn *websocket.Conn, reason string) {
	logger.L.Warnf(s.messageWithPrefix("Closing socket, because: %s"), reason)
	cm := websocket.FormatCloseMessage(websocket.CloseNormalClosure, reason)
	if err := conn.WriteMessage(websocket.CloseMessage, cm); err != nil {
		logger.L.Error(err)
		err = conn.Close()
		if err != nil {
			logger.L.Error(err)
		}
	}
}

func (s *Session) origin() domain.Origin {
	return domain.NewOrigin(s.getUsername(), domain.FromSession)
}

func (s *Session) getUsername() string {
	return strings.Split(s.ID, "@")[0]
}

func (s *Session) messageWithPrefix(str string) string {
	prefix := fmt.Sprintf("SESSION %s: ", s.ID)
	return prefix + str
}
package websocket

import (
	"fmt"
	"github.com/Szetty/seven_wonders/backend/dto"
	"github.com/Szetty/seven_wonders/backend/logger"
	"github.com/Szetty/seven_wonders/backend/users"
	"github.com/google/uuid"
	"github.com/gorilla/websocket"
	"github.com/pkg/errors"
	"strings"
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
)

type RegisterHubEvent struct {
	HubCh chan<- dto.OriginEnvelope
}

type Session struct {
	ID       string
	ClientCh chan<- dto.OriginEnvelope
	EventCh  chan<- interface{}
	internalSession
}

type internalSession struct {
	clientCh <-chan dto.OriginEnvelope
	eventCh  <-chan interface{}
	hubCh    chan<- dto.OriginEnvelope
	conn     *websocket.Conn
	hubQ     []dto.Envelope
	clientQ  []dto.Envelope
}

func newSession(username string, conn *websocket.Conn) *Session {
	id := username + "@" + uuid.New().String()
	clientCh := make(chan dto.OriginEnvelope, 100)
	eventCh := make(chan interface{})
	return &Session{
		ID:       id,
		ClientCh: clientCh,
		EventCh:  eventCh,
		internalSession: internalSession{
			clientCh: clientCh,
			eventCh:  eventCh,
			conn:     conn,
			hubQ:     []dto.Envelope{},
			clientQ:  []dto.Envelope{},
		},
	}
}

func (s *Session) refreshSession(conn *websocket.Conn) {
	if s.conn != nil {
		closeWebsocket(s.conn, "Refreshing session")
		s.invalidateSessionChannels()
	}
	clientCh := make(chan dto.OriginEnvelope, 100)
	s.conn = conn
	s.hubCh = nil
	s.ClientCh = clientCh
	s.clientCh = clientCh
}

func (s *Session) startSession() {
	s.conn.SetReadLimit(maxMessageSize)
	_ = s.conn.SetReadDeadline(time.Now().Add(pongWait))
	s.conn.SetPongHandler(func(string) error { _ = s.conn.SetReadDeadline(time.Now().Add(pongWait)); return nil })
	go s.sessionRoutine()
	s.sendWelcomeMessage()
}

func (s *Session) invalidateSessionChannels() {
}

func (s *Session) sessionRoutine() {
	conn := s.conn
	logger.L.Infof("Starting session %s", s.ID)
	clientReaderCh := make(chan []dto.Envelope)
	hubTicker := time.NewTicker(hubRetryPeriod)
	pingTicker := time.NewTicker(pingPeriod)
	defer func() {
		hubTicker.Stop()
		pingTicker.Stop()
	}()
	go func() {
		for {
			logger.L.Infof("Waiting to receive")
			err, envelopes := ReceiveEnvelopes(conn)
			logger.L.Infof("Received")
			if err != nil {
				if websocket.IsCloseError(errors.Cause(err), websocket.CloseNormalClosure) {
					logger.L.Infof("WS was closed for session %s", s.ID)
					close(clientReaderCh)
					return
				}
				logger.L.Warnf("Fail to read WS message: %v", err)
				time.Sleep(time.Second)
				if websocket.IsUnexpectedCloseError(errors.Cause(err), websocket.CloseNormalClosure) {
					close(clientReaderCh)
					return
				}
				clientReaderCh <- []dto.Envelope{}
			}
			clientReaderCh <- envelopes
		}
	}()
	for {
		select {
		case envelopes, ok := <-clientReaderCh:
			if !ok {
				offlineMessage := dto.MessageBuilder{}.MessageType(dto.UserGotOffline).Body(s.getUsername()).Build()
				s.hubCh <- dto.EnveloperBuilder{}.Data(offlineMessage).GenerateUUID().Build().WithOrigin(s.origin())
				return
			}
			if len(envelopes) <= 0 {
				continue
			}
			logger.L.Infof("Received envelopes %s: %#v", s.ID, envelopes)
			for _, envelope := range envelopes {
				switch m := envelope.Data.(type) {
				case dto.Message:
					s.handleMessage(envelope, m)
				default:
					//TODO implement ACKs and other envelope types
				}
			}

			if s.hubCh != nil {
				if len(s.hubQ) > 0 {
					logger.L.Infof("Sending to hub from reader channel")
					for _, envelope := range s.hubQ {
						s.hubCh <- envelope.WithOrigin(s.origin())
					}
					s.hubQ = []dto.Envelope{}
				}
			} else {
				logger.L.Infof("No hub channel yet")
			}
		case <-hubTicker.C:
			if len(s.hubQ) > 0 {
				logger.L.Infof("Hub queue is NOT empty, trying to send")
				if s.hubCh != nil {
					logger.L.Infof("Sending to hub from queue")
					for _, envelope := range s.hubQ {
						s.hubCh <- envelope.WithOrigin(s.origin())
					}
					s.hubQ = []dto.Envelope{}
				}
			}
		case e := <-s.eventCh:
			switch event := e.(type) {
			case RegisterHubEvent:
				s.hubCh = event.HubCh
			}
		case originEnvelope, ok := <-s.clientCh:
			if !ok {
				clientChannelClosed(conn)
				closeWebsocket(conn, "to client channel was closed")
				return
			}
			_ = conn.SetWriteDeadline(time.Now().Add(writeWait))
			s.clientQ = append(s.clientQ, originEnvelope.Envelope)

			readAllMessagesFromChannel(s, conn)

			logger.L.Infof("Sending envelopes %s: %#v", s.ID, s.clientQ)

			if err := conn.WriteJSON(s.clientQ); err != nil {
				if websocket.IsCloseError(err, websocket.CloseNormalClosure) {
					return
				}
				if websocket.IsUnexpectedCloseError(err) {
					logger.L.Warnf("Fail to write WS message: %v", err)
					return
				}
				reason := fmt.Sprintf("Could not write bundle message to WS: %v", err)
				closeWebsocket(conn, reason)
				return
			}
			s.clientQ = []dto.Envelope{}
		case <-pingTicker.C:
			_ = conn.SetWriteDeadline(time.Now().Add(writeWait))
			if err := conn.WriteMessage(websocket.PingMessage, nil); err != nil {
				if websocket.IsCloseError(err, websocket.CloseNormalClosure) {
					return
				}
				if websocket.IsUnexpectedCloseError(err) {
					logger.L.Warnf("Fail to send ping message: %v", err)
					return
				}
				reason := fmt.Sprintf("Could not write ping message to WS: %v", err)
				closeWebsocket(conn, reason)
				return
			}
		}
	}
}

func (s *Session) sendWelcomeMessage() {
	s.ClientCh <- dto.EnveloperBuilder{}.
		Data(
			dto.MessageBuilder{}.
				MessageType(dto.Welcome).
				Body("").
				Build(),
		).
		UUID(uuid.New().String()).
		Build().
		WithOrigin(dto.NewOrigin("", dto.Empty))
}

func (s *Session) origin() dto.Origin {
	return dto.NewOrigin(s.getUsername(), dto.FromSession)
}

func (s *Session) getUsername() string {
	return strings.Split(s.ID, "@")[0]
}

func (s *Session) handleMessage(envelope dto.Envelope, message dto.Message) {
	switch message.MessageType {
	case dto.OnlineUsers:
		users.GetAllOnline(envelope.WithOrigin(s.origin()))
	default:
		s.hubQ = append(s.hubQ, envelope)
	}
}

func clientChannelClosed(conn *websocket.Conn) {
	logger.L.Info("Client channel was closed")
	_ = conn.WriteMessage(websocket.CloseMessage, []byte{})
}

func readAllMessagesFromChannel(s *Session, conn *websocket.Conn) {
	n := len(s.clientCh)
	for i := 0; i < n; i++ {
		originEnvelope, ok := <-s.clientCh
		if !ok {
			clientChannelClosed(conn)
			return
		}
		s.clientQ = append(s.clientQ, originEnvelope.Envelope)
	}
}

func closeWebsocket(conn *websocket.Conn, reason string) {
	logger.L.Warnf("Closing socket, because: %s", reason)
	cm := websocket.FormatCloseMessage(websocket.CloseNormalClosure, reason)
	if err := conn.WriteMessage(websocket.CloseMessage, cm); err != nil {
		logger.L.Error(err)
		err = conn.Close()
		if err != nil {
			logger.L.Error(err)
		}
	}
}

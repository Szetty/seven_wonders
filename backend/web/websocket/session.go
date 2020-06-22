package websocket

import (
	"fmt"
	"github.com/google/uuid"
	"github.com/gorilla/websocket"
	"github.com/pkg/errors"
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
)

type Session struct {
	id           string
	conn         *websocket.Conn
	fromClientCh chan Envelope
	toClientCh   chan Envelope
	toClientQ    []Envelope
}

func newSession(name string, conn *websocket.Conn) *Session {
	id := name + "@" + uuid.New().String()
	return &Session{
		id:           id,
		conn:         conn,
		fromClientCh: make(chan Envelope),
		toClientCh:   make(chan Envelope),
		toClientQ:    []Envelope{},
	}
}

func (s *Session) refreshSession(conn *websocket.Conn) {
	if s.conn != nil {
		closeWebsocket(s.conn, "Refreshing session")
		s.invalidateSessionChannels()
	}
	s.conn = conn
	s.fromClientCh = make(chan Envelope)
	s.toClientCh = make(chan Envelope)
}

func (s *Session) restartSession() {
	s.conn.SetReadLimit(maxMessageSize)
	_ = s.conn.SetReadDeadline(time.Now().Add(pongWait))
	s.conn.SetPongHandler(func(string) error { _ = s.conn.SetReadDeadline(time.Now().Add(pongWait)); return nil })
	go s.readChannel()
	go s.writeChannel()
	s.sendWelcomeMessage()
}

func (s *Session) invalidateSessionChannels() {
	close(s.fromClientCh)
	close(s.toClientCh)
}

// readChannel sends messages from the websocket connection to the game hub.
//
// The application runs readChannel in a per-connection goroutine. The application
// ensures that there is at most one reader on a connection by executing all
// reads from this goroutine.
func (s *Session) readChannel() {
	conn := s.conn
	logger.Infof("Starting read channel for %s", s.id)
	for {
		err, envelopes := ReceiveEnvelopes(conn)
		if err != nil {
			if websocket.IsCloseError(errors.Cause(err), websocket.CloseNormalClosure) {
				return
			}
			logger.Warnf("Fail to read WS message: %v", err)
			time.Sleep(time.Second)
			if websocket.IsUnexpectedCloseError(errors.Cause(err)) {
				return
			}
			continue
		}
		logger.Infof("Received envelopes %s: %+v", s.id, envelopes)
	}
}

// writeChannel sends messages from the game hub to the websocket connection.
//
// A goroutine running writePump is started for each connection. The
// application ensures that there is at most one writer to a connection by
// executing all writes from this goroutine.
func (s *Session) writeChannel() {
	logger.Infof("Starting write channel for %s", s.id)
	conn := s.conn
	toClientCh := s.toClientCh
	ticker := time.NewTicker(pingPeriod)
	defer func() {
		ticker.Stop()
	}()
	for {
		select {
		case message, ok := <-toClientCh:
			if !ok {
				toClientChannelClosed(conn)
				closeWebsocket(conn, "to client channel was closed")
				return
			}
			_ = conn.SetWriteDeadline(time.Now().Add(writeWait))
			s.toClientQ = append(s.toClientQ, message)

			readAllMessagesFromChannel(s, conn)

			logger.Infof("Sending envelopes %s: %+v", s.id, s.toClientQ)

			if err := conn.WriteJSON(s.toClientQ); err != nil {
				if websocket.IsCloseError(err, websocket.CloseNormalClosure) {
					return
				}
				if websocket.IsUnexpectedCloseError(err) {
					logger.Warnf("Fail to write WS message: %v", err)
					return
				}
				reason := fmt.Sprintf("Could not write bundle message to WS: %v", err)
				closeWebsocket(conn, reason)
				return
			}
			s.toClientQ = []Envelope{}
		case <-ticker.C:
			_ = conn.SetWriteDeadline(time.Now().Add(writeWait))
			if err := conn.WriteMessage(websocket.PingMessage, nil); err != nil {
				if websocket.IsCloseError(err, websocket.CloseNormalClosure) {
					return
				}
				if websocket.IsUnexpectedCloseError(err) {
					logger.Warnf("Fail to send ping message: %v", err)
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
	s.toClientCh <- EnveloperBuilder{}.
		Data(
			MessageBuilder{}.
				MessageType("welcome").
				Body("").
				Build(),
		).
		UUID(uuid.New().String()).
		Build()
}

func toClientChannelClosed(conn *websocket.Conn) {
	logger.Info("Incoming channel was closed")
	_ = conn.WriteMessage(websocket.CloseMessage, []byte{})
}

func readAllMessagesFromChannel(s *Session, conn *websocket.Conn) {
	n := len(s.toClientCh)
	for i := 0; i < n; i++ {
		message, ok := <-s.toClientCh
		if !ok {
			toClientChannelClosed(conn)
			return
		}
		s.toClientQ = append(s.toClientQ, message)
	}
}

func closeWebsocket(conn *websocket.Conn, reason string) {
	logger.Warnf("Closing socket, because: %s", reason)
	cm := websocket.FormatCloseMessage(websocket.CloseNormalClosure, reason)
	if err := conn.WriteMessage(websocket.CloseMessage, cm); err != nil {
		logger.Error(err)
		err = conn.Close()
		if err != nil {
			logger.Error(err)
		}
	}
}
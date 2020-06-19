package websocket

import (
	"github.com/gorilla/websocket"
	"sync"
)

var sessions = sync.Map{}

func findOrCreateSession(name string, conn *websocket.Conn) *Session {
	session, exists := sessions.LoadOrStore(name, newSession(name, conn))
	s := session.(*Session)
	if exists {
		s.refreshSession(conn)
	}
	go s.readChannel()
	go s.writeChannel()
	s.sendWelcomeMessage()
	return s
}
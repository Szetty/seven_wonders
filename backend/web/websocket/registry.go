package websocket

import (
	"github.com/gorilla/websocket"
	"sync"
)

var sessions = sync.Map{}

func findOrCreateSession(username string, conn *websocket.Conn) *Session {
	session, exists := sessions.LoadOrStore(username, newSession(username, conn))
	s := session.(*Session)
	if exists {
		s.refreshSession(conn)
	}
	s.startSession()
	return s
}

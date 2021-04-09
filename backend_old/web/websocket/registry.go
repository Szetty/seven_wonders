package websocket

import (
	"github.com/gorilla/websocket"
	"sync"
)

type Sessions struct {
	byUsername sync.Map
}

func InitSessions() *Sessions {
	return &Sessions{byUsername: sync.Map{}}
}

func (sessions *Sessions) findOrCreateSession(username string, conn *websocket.Conn) (*Session, bool) {
	session, exists := sessions.byUsername.LoadOrStore(username, newSession(username, conn))
	s := session.(*Session)
	var wasOnline = false
	if exists {
		wasOnline = s.refreshSession(conn)
	}
	s.startSession()
	return s, wasOnline
}

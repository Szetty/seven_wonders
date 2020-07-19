package users

import (
	"github.com/Szetty/seven_wonders/backend/dto"
	"github.com/Szetty/seven_wonders/backend/logger"
)

type Users struct {
	onlineUsers map[string]chan<- dto.OriginEnvelope
	eventCh     chan interface{}
}

var u = &Users{
	onlineUsers: make(map[string]chan<- dto.OriginEnvelope),
	eventCh:     make(chan interface{}),
}

func init() {
	go u.usersRoutine()
}

type getAllOnline struct {
	envelope dto.OriginEnvelope
}

type register struct {
	username  string
	sessionCh chan<- dto.OriginEnvelope
}

type notify struct {
	username string
	message  dto.Message
	origin   dto.Origin
}

type unregister struct {
	username string
}

func GetAllOnline(envelope dto.OriginEnvelope) {
	u.eventCh <- getAllOnline{envelope}
}

func Register(username string, sessionCh chan<- dto.OriginEnvelope) {
	u.eventCh <- register{username, sessionCh}
}

func Unregister(username string) {
	u.eventCh <- unregister{username}
}

func Notify(username string, message dto.Message, origin dto.Origin) {
	u.eventCh <- notify{username, message, origin}
}

func (u *Users) usersRoutine() {
	for {
		select {
		case e := <-u.eventCh:
			switch event := e.(type) {
			case register:
				for _, session := range u.onlineUsers {
					message := dto.MessageBuilder{}.MessageType(dto.UserGotOnline).Body(event.username).Build()
					dto.NotifyTarget(session, message, u.origin())
				}
				u.onlineUsers[event.username] = event.sessionCh
			case unregister:
				close(u.onlineUsers[event.username])
				delete(u.onlineUsers, event.username)
				for _, session := range u.onlineUsers {
					message := dto.MessageBuilder{}.MessageType(dto.UserGotOffline).Body(event.username).Build()
					dto.NotifyTarget(session, message, u.origin())
				}
			case getAllOnline:
				var onlineUsersList []string
				for username, _ := range u.onlineUsers {
					onlineUsersList = append(onlineUsersList, username)
				}
				replyMessage := dto.MessageBuilder{}.MessageType(dto.OnlineUsersReply).Body(onlineUsersList).Build()
				dto.ReplyToOrigin(u.onlineUsers[event.envelope.ID], event.envelope, replyMessage, u.origin())
			case notify:
				u.notifyTarget(event.username, event.message, event.origin)
			}
		}
	}
}

func (u *Users) notifyTarget(username string, message dto.Message, origin dto.Origin) {
	sessionCh, exists := u.onlineUsers[username]
	if exists {
		dto.NotifyTarget(sessionCh, message, origin)
	} else {
		logger.L.Warnf("Cannot notify target %s", username)
	}
}

func (u *Users) origin() dto.Origin {
	return dto.NewOrigin("", dto.FromUsers)
}

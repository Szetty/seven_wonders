package user

import (
	"github.com/Szetty/seven_wonders/backend_old/domain"
	"github.com/Szetty/seven_wonders/backend_old/logger"
)

type Crux struct {
	onlineUsers map[string]chan<- domain.OriginEnvelope
	eventCh     chan interface{}
	envelopeCh  chan domain.OriginEnvelope
}

func Init() *Crux {
	c := &Crux{
		onlineUsers: make(map[string]chan<- domain.OriginEnvelope),
		eventCh:     make(chan interface{}),
		envelopeCh:  make(chan domain.OriginEnvelope),
	}
	go c.usersRoutine()
	return c
}

// events
type (
	register struct {
		username  string
		sessionCh chan<- domain.OriginEnvelope
	}
	update struct {
		username  string
		sessionCh chan<- domain.OriginEnvelope
	}
	notify struct {
		username string
		message  domain.Message
		origin   domain.Origin
	}
	unregister struct {
		username string
	}
)

func (c *Crux) Register(username string, sessionCh chan<- domain.OriginEnvelope) {
	c.eventCh <- register{username, sessionCh}
}

func (c* Crux) Update(username string, sessionCh chan<- domain.OriginEnvelope) {
	c.eventCh <- update{username, sessionCh}
}

func (c *Crux) Unregister(username string) {
	c.eventCh <- unregister{username}
}

func (c *Crux) Notify(username string, message domain.Message, origin domain.Origin) {
	c.eventCh <- notify{username, message, origin}
}

func (c *Crux) Channel() chan<- domain.OriginEnvelope {
	return c.envelopeCh
}

func (c *Crux) usersRoutine() {
	for {
		select {
		case e := <-c.eventCh:
			logger.L.Infof(c.messageWithPrefix("Got event: %#v"), e)
			switch event := e.(type) {
			case register:
				for _, session := range c.onlineUsers {
					message := domain.MessageBuilder{}.MessageType(domain.GotOnline).Body(event.username).Build()
					domain.NotifyTarget(session, message, c.origin())
				}
				c.onlineUsers[event.username] = event.sessionCh
			case unregister:
				close(c.onlineUsers[event.username])
				delete(c.onlineUsers, event.username)
				for _, session := range c.onlineUsers {
					message := domain.MessageBuilder{}.MessageType(domain.GotOffline).Body(event.username).Build()
					domain.NotifyTarget(session, message, c.origin())
				}
			case update:
				c.onlineUsers[event.username] = event.sessionCh
			case notify:
				c.notifyTarget(event.username, event.message, event.origin)
			}
		case env := <-c.envelopeCh:
			c.handleEnvelope(env)
		}
	}
}

func (c *Crux) handleEnvelope(originEnvelope domain.OriginEnvelope) {
	logger.L.Infof(c.messageWithPrefix("ENVELOPE: %#v"), originEnvelope)
	switch m := originEnvelope.Data.(type) {
	case domain.Message:
		switch m.MessageType {
		case domain.OnlineUsers:
			var onlineUsersList []string
			for username, _ := range c.onlineUsers {
				onlineUsersList = append(onlineUsersList, username)
			}
			replyMessage := domain.MessageBuilder{}.MessageType(domain.OnlineUsersReply).Body(onlineUsersList).Build()
			domain.ReplyToOrigin(c.onlineUsers[originEnvelope.ID], originEnvelope, replyMessage, c.origin())
		default:
			logger.L.Warnf(c.messageWithPrefix("Unknown message type: %s"), m.MessageType)
		}
	default:
		logger.L.Warnf(c.messageWithPrefix("Unknown origin envelope: %v"), originEnvelope)
	}
}

func (c *Crux) notifyTarget(username string, message domain.Message, origin domain.Origin) {
	sessionCh, exists := c.onlineUsers[username]
	if exists {
		domain.NotifyTarget(sessionCh, message, origin)
	} else {
		logger.L.Warnf("Cannot notify target %s", username)
	}
}

func (c *Crux) origin() domain.Origin {
	return domain.NewOrigin("", domain.FromUsers)
}

func (c *Crux) messageWithPrefix(s string) string {
	return "USER_CRUX: " + s
}

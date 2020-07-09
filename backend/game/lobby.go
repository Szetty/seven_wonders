package game

import (
	"github.com/Szetty/seven_wonders/backend/dto"
	"sync"
)

type Lobby struct {
	gameID            string
	authorizedUsers   map[string]bool
	connectedUsers    map[string]bool
	connectedSessions map[string]chan dto.OriginEnvelope
	envelopeCh        chan dto.OriginEnvelope
	eventCh           chan interface{}
}

type register struct {
	username    string
	toSessionCh chan dto.OriginEnvelope
}

type changeAuthorization struct {
	username string
	value    bool
}

type verifyAuthorization struct {
	username string
	replyCh  chan bool
}

var gameLobbies = sync.Map{}
var onlineUsers = sync.Map{}

func RegisterInLobby(username, gameID string, toSessionCh chan dto.OriginEnvelope) chan dto.OriginEnvelope {
	onlineUsers.Store(username, toSessionCh)
	l, exists := gameLobbies.LoadOrStore(gameID, newLobby(gameID))
	lobby := l.(*Lobby)
	if !exists {
		go lobby.lobby()
	}
	lobby.eventCh <- register{username, toSessionCh}
	return lobby.envelopeCh
}

func AuthorizedForLobby(username, gameID string) bool {
	l, exists := gameLobbies.Load(gameID)
	if !exists {
		return false
	}
	lobby := l.(*Lobby)
	replyCh := make(chan bool)
	lobby.eventCh <- verifyAuthorization{username, replyCh}
	return <-replyCh
}

func newLobby(gameID string) *Lobby {
	return &Lobby{
		gameID:            gameID,
		authorizedUsers:   make(map[string]bool),
		connectedUsers:    make(map[string]bool),
		connectedSessions: make(map[string]chan dto.OriginEnvelope),
		envelopeCh:        make(chan dto.OriginEnvelope),
		eventCh:           make(chan interface{}),
	}
}

func (l *Lobby) lobby() {
	for {
		select {
		case envelope := <-l.envelopeCh:
			logger.Infof("On lobby %s got envelope: %#v", l.gameID, envelope)
			l.handleEnvelope(envelope)
		case e := <-l.eventCh:
			logger.Infof("On lobby %s got event: %#v", l.gameID, e)
			switch event := e.(type) {
			case register:
				l.registerUserAndSession(event.username, event.toSessionCh)
			case changeAuthorization:
				l.changeAuthorization(event.username, event.value)
			case verifyAuthorization:
				event.replyCh <- l.authorizedUsers[event.username]
			}
		}
	}
}

func (l *Lobby) handleEnvelope(originEnvelope dto.OriginEnvelope) {
	switch m := originEnvelope.Data.(type) {
	case dto.Message:
		switch m.MessageType {
		case dto.OnlineUsers:
			var onlineUsersList []string
			onlineUsers.Range(func(key, value interface{}) bool {
				onlineUsersList = append(onlineUsersList, key.(string))
				return true
			})
			replyMessage := dto.MessageBuilder{}.MessageType(dto.OnlineUsersReply).Body(onlineUsersList).Build()
			l.replyToOrigin(originEnvelope, replyMessage)
		case dto.InviteUser:
			toInvite := m.Body.(string)
			l.notifyTarget(toInvite, dto.MessageBuilder{}.MessageType(dto.GotInvite).Body(l.gameID).Build())
			l.replyToOrigin(originEnvelope, dto.MessageBuilder{}.MessageType(dto.InviteUserReply).Body(toInvite).Build())
			l.changeAuthorization(toInvite, true)
		case dto.InvitedUsers:
			var invitedUsers []dto.InvitedUser
			for name, invited := range l.authorizedUsers {
				if invited {
					invitedUsers = append(invitedUsers, dto.InvitedUser{Name: name, Connected: l.connectedUsers[name]})
				}
			}
			replyMessage := dto.MessageBuilder{}.MessageType(dto.InvitedUsersReply).Body(invitedUsers).Build()
			l.replyToOrigin(originEnvelope, replyMessage)
		case dto.UninviteUser:
			toUninvite := m.Body.(string)
			l.notifyTarget(toUninvite, dto.MessageBuilder{}.MessageType(dto.GotUninvite).Body(l.gameID).Build())
			l.replyToOrigin(originEnvelope, dto.MessageBuilder{}.MessageType(dto.UninviteUserReply).Body(toUninvite).Build())
			l.changeAuthorization(toUninvite, false)
		case dto.StartGame:
			//TODO implement starting a game
			break
		default:
			logger.Warnf("Unknown message type: %s", m.MessageType)
		}
	default:
		logger.Warnf("Unknown origin envelope: %v", originEnvelope)
	}
}

func (l *Lobby) registerUserAndSession(username string, toSessionCh chan dto.OriginEnvelope) {
	l.connectedUsers[username] = true
	if l.connectedSessions[username] != nil {
		close(l.connectedSessions[username])
	}
	l.connectedSessions[username] = toSessionCh
}

func (l *Lobby) origin() dto.Origin {
	return dto.NewOrigin(l.gameID, dto.FromLobby)
}

func (l *Lobby) replyToOrigin(envelope dto.OriginEnvelope, replyMessage dto.Message) {
	switch envelope.OriginType {
	case dto.FromSession:
		l.connectedSessions[envelope.ID] <- dto.Reply(envelope.Envelope, replyMessage).WithOrigin(l.origin())
	default:
		logger.Warnf("Unknown origin type: %s", envelope.OriginType)
	}
}

func (l *Lobby) notifyTarget(username string, message dto.Message) {
	sessionCh, exists := onlineUsers.Load(username)
	if exists {
		sessionCh.(chan dto.OriginEnvelope) <- dto.Notify(message).WithOrigin(l.origin())
	} else {
		logger.Warnf("Cannot notify target %s", username)
	}
}

func (l *Lobby) changeAuthorization(username string, value bool) {
	l.authorizedUsers[username] = value
}

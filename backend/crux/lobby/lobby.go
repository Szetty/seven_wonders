package lobby

import (
	"fmt"
	"github.com/Szetty/seven_wonders/backend/crux/auth"
	"github.com/Szetty/seven_wonders/backend/crux/user"
	"github.com/Szetty/seven_wonders/backend/domain"
	"github.com/Szetty/seven_wonders/backend/logger"
	"sync"
)

type Crux struct {
	byGameID sync.Map
	userCrux *user.Crux
	authCrux *auth.Crux
}

func Init(userCrux *user.Crux, authCrux *auth.Crux) *Crux {
	return &Crux{
		byGameID: sync.Map{},
		userCrux: userCrux,
		authCrux: authCrux,
	}
}

type Lobby struct {
	lobbyCrux         *Crux
	gameID            string
	authorizedUsers   map[string]bool
	connectedUsers    map[string]bool
	connectedSessions map[string]chan<- domain.OriginEnvelope
	envelopeCh        chan domain.OriginEnvelope
	eventCh           chan interface{}
}

// events
type (
	register struct {
		username    string
		toSessionCh chan<- domain.OriginEnvelope
	}

	verifyAuthorization struct {
		username string
		replyCh  chan bool
	}
)

func (c *Crux) RegisterInLobby(username, gameID string, toSessionCh chan<- domain.OriginEnvelope) chan<- domain.OriginEnvelope {
	l, exists := c.byGameID.LoadOrStore(gameID, newLobby(c, username, gameID))
	lobby := l.(*Lobby)
	if !exists {
		go lobby.lobbyRoutine()
	}
	lobby.eventCh <- register{username, toSessionCh}
	return lobby.envelopeCh
}

func (c *Crux) AuthorizedForLobby(username, gameID string) bool {
	l, exists := c.byGameID.Load(gameID)
	if !exists {
		return false
	}
	lobby := l.(*Lobby)
	replyCh := make(chan bool)
	lobby.eventCh <- verifyAuthorization{username, replyCh}
	return <-replyCh
}

func (c *Crux) messageWithPrefix(s string) string {
	return "LOBBY_CRUX: " + s
}

func newLobby(lobbiesCrux *Crux, username, gameID string) *Lobby {
	authorizedUsers := make(map[string]bool)
	authorizedUsers[username] = true
	return &Lobby{
		lobbyCrux:         lobbiesCrux,
		gameID:            gameID,
		authorizedUsers:   authorizedUsers,
		connectedUsers:    make(map[string]bool),
		connectedSessions: make(map[string]chan<- domain.OriginEnvelope),
		envelopeCh:        make(chan domain.OriginEnvelope),
		eventCh:           make(chan interface{}),
	}
}

func (l *Lobby) lobbyRoutine() {
	for {
		select {
		case envelope := <-l.envelopeCh:
			logger.L.Infof(l.messageWithPrefix("got envelope: %#v"), envelope)
			l.handleEnvelope(envelope)
		case e := <-l.eventCh:
			logger.L.Infof(l.messageWithPrefix("got event: %#v"), e)
			switch event := e.(type) {
			case register:
				l.registerUserAndSession(event.username, event.toSessionCh)
			case verifyAuthorization:
				event.replyCh <- l.authorizedUsers[event.username]
			}
		}
	}
}

func (l *Lobby) handleEnvelope(originEnvelope domain.OriginEnvelope) {
	switch m := originEnvelope.Data.(type) {
	case domain.Message:
		switch m.MessageType {
		case domain.InviteUser:
			toInvite := m.Body.(string)
			u := domain.User{Name: originEnvelope.ID, GameID: l.gameID}
			l.lobbyCrux.userCrux.Notify(toInvite, domain.MessageBuilder{}.MessageType(domain.GotInvite).Body(u).Build(), l.origin())
			l.replyToOrigin(originEnvelope, domain.MessageBuilder{}.MessageType(domain.InviteUserReply).Body(toInvite).Build())
			l.changeAuthorization(toInvite, true)
		case domain.InvitedUsers:
			invitedUsers := []domain.InvitedUser{}
			leader := l.lobbyCrux.authCrux.UserByGameID(l.gameID)
			for name, invited := range l.authorizedUsers {
				if invited {
					invitedUsers = append(invitedUsers, domain.InvitedUser{
						Name:      name,
						Connected: l.connectedUsers[name],
						Leader:    name == leader,
					})
				}
			}
			replyMessage := domain.MessageBuilder{}.MessageType(domain.InvitedUsersReply).Body(invitedUsers).Build()
			l.replyToOrigin(originEnvelope, replyMessage)
		case domain.UninviteUser:
			toUninvite := m.Body.(string)
			l.lobbyCrux.userCrux.Notify(toUninvite, domain.MessageBuilder{}.MessageType(domain.GotUninvite).Body(l.gameID).Build(), l.origin())
			l.replyToOrigin(originEnvelope, domain.MessageBuilder{}.MessageType(domain.UninviteUserReply).Body(toUninvite).Build())
			l.changeAuthorization(toUninvite, false)
		case domain.GotOffline:
			offlineUsername := originEnvelope.ID
			delete(l.connectedSessions, offlineUsername)
			delete(l.connectedUsers, offlineUsername)
			l.lobbyCrux.userCrux.Unregister(offlineUsername)
		case domain.StartGame:
			//TODO implement starting a game
			break
		default:
			logger.L.Warnf(l.messageWithPrefix("Unknown message type: %s"), m.MessageType)
		}
	default:
		logger.L.Warnf(l.messageWithPrefix("Unknown origin envelope: %v"), originEnvelope)
	}
}

func (l *Lobby) registerUserAndSession(username string, toSessionCh chan<- domain.OriginEnvelope) {
	l.connectedUsers[username] = true
	l.connectedSessions[username] = toSessionCh
}

func (l *Lobby) origin() domain.Origin {
	return domain.NewOrigin(l.gameID, domain.FromLobby)
}

func (l *Lobby) replyToOrigin(envelope domain.OriginEnvelope, replyMessage domain.Message) {
	switch envelope.OriginType {
	case domain.FromSession:
		l.connectedSessions[envelope.ID] <- domain.Reply(envelope.Envelope, replyMessage).WithOrigin(l.origin())
	default:
		logger.L.Warnf(l.messageWithPrefix("Unknown origin type: %s"), envelope.OriginType)
	}
}

func (l *Lobby) changeAuthorization(username string, value bool) {
	l.authorizedUsers[username] = value
}

func (l *Lobby) messageWithPrefix(s string) string {
	lobbyPrefix := fmt.Sprintf("LOBBY %s: ", l.gameID)
	return l.lobbyCrux.messageWithPrefix(lobbyPrefix + s)
}

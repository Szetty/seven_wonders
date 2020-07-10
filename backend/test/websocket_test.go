package test

import (
	"fmt"
	"github.com/Szetty/seven_wonders/backend/dto"
	"github.com/Szetty/seven_wonders/backend/users"
	"github.com/Szetty/seven_wonders/backend/web"
	"github.com/Szetty/seven_wonders/backend/web/websocket"
	"github.com/google/uuid"
	gWebsocket "github.com/gorilla/websocket"
	"net/http/httptest"
	"runtime/debug"
	"strings"
	"testing"
	"time"
)

const receiveTimeout = 1 * time.Second

type wsContext struct {
	ws       *gWebsocket.Conn
	username string
	gameID   string
}

func TestWebSocket(t *testing.T) {
	server := httptest.NewServer(web.MainHandler())
	defer server.Close()
	ctx := setupWS(t, server, "/api/secured/game/")
	defer cleanupWS(t, ctx.ws)
	t.Run("receive welcome message", receiveWelcomeMessageTestFn(ctx.ws))
	t.Run("sending message", func(t *testing.T) {
		envelope := dto.EnveloperBuilder{}.
			Data(
				dto.MessageBuilder{}.
					MessageType(dto.Welcome).
					Body("").
					Build(),
			).
			UUID(uuid.New().String()).
			Build()
		sendEnvelope(t, ctx.ws, envelope)
	})
}

func TestGameLobby(t *testing.T) {
	server := httptest.NewServer(web.MainHandler())
	defer server.Close()
	ctx1 := setupWS(t, server, "/api/secured/game/lobby/")
	ctx2 := setupWS(t, server, "/api/secured/game/lobby/")
	defer cleanupWS(t, ctx1.ws)
	t.Run("receive welcome message user 1", receiveWelcomeMessageTestFn(ctx1.ws))
	t.Run("receive welcome message user 2", receiveWelcomeMessageTestFn(ctx2.ws))
	t.Run("get online users", func(t *testing.T) {
		requestEnvelope := envelopeWithMessageType(dto.OnlineUsers)
		sendEnvelope(t, ctx1.ws, requestEnvelope)
		replyEnvelope := receiveAndVerifyEnvelopes(t, ctx1.ws, 1)[0]
		expectAckUUIDsToContain(t, replyEnvelope.AckUUIDs, requestEnvelope.UUID)
		message := replyEnvelope.Data.(dto.Message)
		expectMessageType(t, message, dto.OnlineUsersReply)
		onlineUsers := message.Body.([]string)
		if len(onlineUsers) != 2 {
			t.Fatalf("Expecting two online users")
		}
		allOnlineUsersStr := strings.Join(onlineUsers, "")
		expectOnlineUser := func(username string) {
			if !strings.Contains(allOnlineUsersStr, username) {
				t.Fatalf("Online users list (%s) does not contain user: %s", allOnlineUsersStr, username)
			}
		}
		expectOnlineUser(ctx1.username)
		expectOnlineUser(ctx2.username)
	})
	t.Run("invite users", func(t *testing.T) {
		invitedUsersRequest1 := envelopeWithMessageType(dto.InvitedUsers)
		sendEnvelope(t, ctx1.ws, invitedUsersRequest1)
		invitedUsersReply1 := receiveAndVerifyEnvelopes(t, ctx1.ws, 1)[0]
		expectAckUUIDsToContain(t, invitedUsersReply1.AckUUIDs, invitedUsersRequest1.UUID)
		invitedUsersMessage1 := invitedUsersReply1.Data.(dto.Message)
		expectMessageType(t, invitedUsersMessage1, dto.InvitedUsersReply)
		invitedUsers1 := invitedUsersMessage1.Body.([]dto.InvitedUser)
		if len(invitedUsers1) != 0 {
			t.Fatalf("Expecting no invited users")
		}

		inviteUserRequest := envelopeWithMessageTypeAndBody(dto.InviteUser, ctx2.username)
		sendEnvelope(t, ctx1.ws, inviteUserRequest)
		inviteUserReply := receiveAndVerifyEnvelopes(t, ctx1.ws, 1)[0]
		expectAckUUIDsToContain(t, inviteUserReply.AckUUIDs, inviteUserRequest.UUID)
		inviteUserMessage := inviteUserReply.Data.(dto.Message)
		expectMessageType(t, inviteUserMessage, dto.InviteUserReply)

		invitedUsersRequest2 := envelopeWithMessageType(dto.InvitedUsers)
		sendEnvelope(t, ctx1.ws, invitedUsersRequest2)
		invitedUsersReply2 := receiveAndVerifyEnvelopes(t, ctx1.ws, 1)[0]
		expectAckUUIDsToContain(t, invitedUsersReply2.AckUUIDs, invitedUsersRequest2.UUID)
		invitedUsersMessage2 := invitedUsersReply2.Data.(dto.Message)
		expectMessageType(t, invitedUsersMessage2, dto.InvitedUsersReply)
		invitedUsers2 := invitedUsersMessage2.Body.([]dto.InvitedUser)
		if len(invitedUsers2) != 1 {
			t.Fatalf("Expecting one invited user")
		}
		if invitedUsers2[0].Name != ctx2.username {
			t.Fatalf("Wrong invited user" + expectedAndGotMessage(invitedUsers2[0].Name, ctx2.username))
		}

		gotInviteEnvelope := receiveAndVerifyEnvelopes(t, ctx2.ws, 1)[0]
		gotInviteMessage := gotInviteEnvelope.Data.(dto.Message)
		expectMessageType(t, gotInviteMessage, dto.GotInvite)
		invitedGameID := gotInviteMessage.Body.(string)
		if invitedGameID != ctx1.gameID {
			t.Fatalf("Wrong invited game id" + expectedAndGotMessage(invitedGameID, ctx1.gameID))
		}
	})
}

func setupWS(t *testing.T, server *httptest.Server, wsPath string) *wsContext {
	baseWSURL := strings.TrimPrefix(server.URL, "http")
	username := "user-" + uuid.New().String()
	err, jwt, gameID := users.CreateUser(username)
	if err != nil {
		t.Fatalf("could not create JWT token: %v", err)
	}
	wsURL := fmt.Sprintf("ws%s%s%s?authorization=%s", baseWSURL, wsPath, gameID, jwt)
	ws, _, err := gWebsocket.DefaultDialer.Dial(wsURL, nil)
	if err != nil {
		t.Fatalf("could not open a WS connection on %s %v", wsURL, err)
	}
	return &wsContext{ws, username, gameID}
}

func receiveWelcomeMessageTestFn(ws *gWebsocket.Conn) func(t *testing.T) {
	return func(t *testing.T) {
		envelopes := receiveAndVerifyEnvelopes(t, ws, 1)
		msg := envelopes[0].Data.(dto.Message)
		if msg.MessageType != dto.Welcome {
			t.Fatalf("received message has wrong type: %s", msg.MessageType)
		}
	}
}

func envelopeWithMessageType(messageType dto.MessageType) dto.Envelope {
	return dto.
		EnveloperBuilder{}.
		Data(dto.MessageBuilder{}.MessageType(messageType).Build()).
		UUID(uuid.New().String()).
		Build()
}

func envelopeWithMessageTypeAndBody(messageType dto.MessageType, body interface{}) dto.Envelope {
	return dto.
		EnveloperBuilder{}.
		Data(dto.MessageBuilder{}.MessageType(messageType).Body(body).Build()).
		UUID(uuid.New().String()).
		Build()
}

func sendEnvelope(t *testing.T, ws *gWebsocket.Conn, envelope dto.Envelope) {
	err := ws.WriteJSON([]dto.Envelope{envelope})
	if err != nil {
		t.Fatalf("writing to WS failed: %v", err)
	}
}

func receiveAndVerifyEnvelopes(t *testing.T, ws *gWebsocket.Conn, expectedSize int) []dto.Envelope {
	type result struct {
		error     error
		envelopes []dto.Envelope
	}
	ch := make(chan result)
	ticker := time.NewTicker(receiveTimeout)
	go func() {
		err, envelopes := websocket.ReceiveEnvelopes(ws)
		ch <- result{err, envelopes}
	}()
	select {
	case r := <-ch:
		if r.error != nil {
			t.Fatalf("could not receive envelopes: %v", r.error)
		}
		if len(r.envelopes) != expectedSize {
			t.Fatalf("got bundle bigger than size 1: %v", r.envelopes)
		}
		for _, envelope := range r.envelopes {
			if envelope.UUID == "" && len(envelope.AckUUIDs) <= 0 {
				t.Fatalf("message has no UUID and AckUUIDs")
			}
		}
		return r.envelopes
	case <-ticker.C:
		t.Fatalf("Received timeout %s", debug.Stack())
		return nil
	}
}

func expectAckUUIDsToContain(t *testing.T, ackUUIDs []string, uuid string) {
	found := false
	for _, ackUUID := range ackUUIDs {
		if ackUUID == uuid {
			found = true
			break
		}
	}
	if !found {
		expectedGot := expectedAndGotMessage(strings.Join(ackUUIDs, ""), uuid)
		t.Fatalf("Expected uuid is not present in ack uuids"+expectedGot+"%s", debug.Stack())
	}
}

func expectMessageType(t *testing.T, message dto.Message, messageType dto.MessageType) {
	if message.MessageType != messageType {
		t.Fatalf("Unexpected message type" + expectedAndGotMessage(message.MessageType, messageType))
	}
}

func cleanupWS(t *testing.T, ws *gWebsocket.Conn) {
	cm := gWebsocket.FormatCloseMessage(gWebsocket.CloseNormalClosure, "done")
	if err := ws.WriteMessage(gWebsocket.CloseMessage, cm); err != nil {
		t.Fatalf("could not send close message: %v", err)
	}
	_ = ws.Close()
}

func expectedAndGotMessage(args ...interface{}) string {
	return fmt.Sprintf(", got: %s, expected: %s", args...)
}

package test

import (
	"github.com/Szetty/seven_wonders/backend/domain"
	"github.com/Szetty/seven_wonders/backend/web"
	"github.com/google/uuid"
	gWebsocket "github.com/gorilla/websocket"
	"net/http/httptest"
	"strings"
	"testing"
	"time"
)

func TestWebSocket(t *testing.T) {
	server := httptest.NewServer(web.MainHandler())
	defer server.Close()
	ctx := setupWS(t, server, "/api/secured/game/")
	defer cleanupWS(t, ctx.ws)
	expectWelcomeMessage(t, ctx.ws)
	envelope := domain.EnveloperBuilder{}.
		Data(
			domain.MessageBuilder{}.
				MessageType(domain.Welcome).
				Body("").
				Build(),
		).
		UUID(uuid.New().String()).
		Build()
	sendEnvelope(t, ctx.ws, envelope)
}

func TestOnlineUsers(t *testing.T) {
	server := httptest.NewServer(web.MainHandler())
	defer server.Close()
	ctx1 := setupWS(t, server, "/api/secured/game/lobby/")
	ctx2 := setupWS(t, server, "/api/secured/game/lobby/")
	defer cleanupWS(t, ctx1.ws)
	defer cleanupWS(t, ctx2.ws)
	expectWelcomeMessage(t, ctx1.ws)
	expectWelcomeMessage(t, ctx2.ws)
	expectUserGotOnline(t, ctx1.ws, ctx2.username)
	requestEnvelope := envelopeWithMessageType(domain.OnlineUsers)
	sendEnvelope(t, ctx1.ws, requestEnvelope)
	replyEnvelope := receiveAndVerifyEnvelopes(t, ctx1.ws, 1, false)[0]
	expectAckUUIDsToContain(t, replyEnvelope.AckUUIDs, requestEnvelope.UUID)
	message := replyEnvelope.Data.(domain.Message)
	expectMessageType(t, message, domain.OnlineUsersReply)
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
}

func TestOfflineUsers(t *testing.T) {
	server := httptest.NewServer(web.MainHandler())
	defer server.Close()
	ctx1 := setupWS(t, server, "/api/secured/game/lobby/")
	defer cleanupWS(t, ctx1.ws)
	expectWelcomeMessage(t, ctx1.ws)
	ctx2 := setupWS(t, server, "/api/secured/game/lobby/")
	expectWelcomeMessage(t, ctx2.ws)
	expectUserGotOnline(t, ctx1.ws, ctx2.username)
	ctx3 := setupWS(t, server, "/api/secured/game/lobby/")
	defer cleanupWS(t, ctx3.ws)
	expectWelcomeMessage(t, ctx3.ws)
	expectUserGotOnline(t, ctx1.ws, ctx3.username)

	cleanupWS(t, ctx2.ws)
	receiveAndVerifyEnvelopes(t, ctx3.ws, 1, true)
	time.Sleep(5000 * time.Millisecond)
	expectUserGotOffline(t, ctx1.ws, ctx2.username)
}

func TestReconnect(t *testing.T) {
	server := httptest.NewServer(web.MainHandler())
	defer server.Close()
	ctx1 := setupWS(t, server, "/api/secured/game/lobby/")
	defer cleanupWS(t, ctx1.ws)
	expectWelcomeMessage(t, ctx1.ws)
	ctx2 := setupWS(t, server, "/api/secured/game/lobby/")
	expectWelcomeMessage(t, ctx2.ws)
	expectUserGotOnline(t, ctx1.ws, ctx2.username)

	cleanupWS(t, ctx2.ws)
	ctx2.reconnectWS(t)
	defer cleanupWS(t, ctx2.ws)
	expectWelcomeMessage(t, ctx2.ws)
	receiveAndVerifyEnvelopes(t, ctx1.ws, 1, true)
}

func expectUserGotOnline(t *testing.T, ws *gWebsocket.Conn, expectedUsername string) {
	envelopes := receiveAndVerifyEnvelopes(t, ws, 1, false)
	msg := envelopes[0].Data.(domain.Message)
	if msg.MessageType != domain.GotOnline {
		t.Fatalf("expected user got online" + gotAndExpectedMessage(msg.MessageType, domain.GotOnline))
	}
	gotUsername := msg.Body.(string)
	if gotUsername != expectedUsername {
		t.Fatalf("user got online" + gotAndExpectedMessage(gotUsername, expectedUsername))
	}
}

func expectUserGotOffline(t *testing.T, ws *gWebsocket.Conn, expectedUsername string) {
	envelopes := receiveAndVerifyEnvelopes(t, ws, 1, false)
	msg := envelopes[0].Data.(domain.Message)
	if msg.MessageType != domain.GotOffline {
		t.Fatalf("expected user got offline" + gotAndExpectedMessage(msg.MessageType, domain.GotOffline))
	}
	gotUsername := msg.Body.(string)
	if gotUsername != expectedUsername {
		t.Fatalf("user got offline" + gotAndExpectedMessage(gotUsername, expectedUsername))
	}
}

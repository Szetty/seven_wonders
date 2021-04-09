package test

import (
	"github.com/Szetty/seven_wonders/backend_old/domain"
	"github.com/Szetty/seven_wonders/backend_old/web"
	"github.com/google/uuid"
	"net/http/httptest"
	"strings"
	"testing"
	"time"
)

func TestWebSocket(t *testing.T) {
	server := httptest.NewServer(web.MainHandler())
	defer server.Close()
	ctx := setupWS(t, server, "/api/secured/game/lobby/")
	defer ctx.close(t)
	ctx.expectWelcomeMessage(t)
	envelope := domain.EnveloperBuilder{}.
		Data(
			domain.MessageBuilder{}.
				MessageType(domain.Welcome).
				Body("").
				Build(),
		).
		UUID(uuid.New().String()).
		Build()
	ctx.sendEnvelope(t, envelope)
}

func TestOnlineUsers(t *testing.T) {
	server := httptest.NewServer(web.MainHandler())
	defer server.Close()
	ctx1 := setupWS(t, server, "/api/secured/game/lobby/")
	ctx2 := setupWS(t, server, "/api/secured/game/lobby/")
	defer ctx1.close(t)
	defer ctx2.close(t)
	ctx1.expectWelcomeMessage(t)
	ctx2.expectWelcomeMessage(t)
	ctx1.expectUserGotOnline(t, ctx2.username)
	testOnlineUsers(t, ctx1, []string{ctx1.username, ctx2.username})
}

func TestOfflineUsers(t *testing.T) {
	server := httptest.NewServer(web.MainHandler())
	defer server.Close()
	ctx1 := setupWS(t, server, "/api/secured/game/lobby/")
	defer ctx1.close(t)
	ctx1.expectWelcomeMessage(t)
	ctx2 := setupWS(t, server, "/api/secured/game/lobby/")
	ctx2.expectWelcomeMessage(t)
	ctx1.expectUserGotOnline(t, ctx2.username)
	ctx3 := setupWS(t, server, "/api/secured/game/lobby/")
	defer ctx3.close(t)
	ctx3.expectWelcomeMessage(t)
	ctx1.expectUserGotOnline(t, ctx3.username)

	ctx2.close(t)
	ctx3.receiveAndVerifyEnvelopes(t, 1, true)
	time.Sleep(5000 * time.Millisecond)
	ctx1.expectUserGotOffline(t, ctx2.username)
}

func TestReconnect(t *testing.T) {
	server := httptest.NewServer(web.MainHandler())
	defer server.Close()
	ctx1 := setupWS(t, server, "/api/secured/game/lobby/")
	defer ctx1.close(t)
	ctx1.expectWelcomeMessage(t)
	ctx2 := setupWS(t, server, "/api/secured/game/lobby/")
	ctx2.expectWelcomeMessage(t)
	ctx1.expectUserGotOnline(t, ctx2.username)

	ctx2.close(t)
	ctx2.reconnectWS(t)
	defer ctx2.close(t)
	ctx2.expectWelcomeMessage(t)
	ctx1.receiveAndVerifyEnvelopes(t, 1, true)
}

func testOnlineUsers(t *testing.T, ctx *wsContext, usernames []string) {
	requestEnvelope := envelopeWithMessageType(domain.OnlineUsers)
	ctx.sendEnvelope(t, requestEnvelope)
	replyEnvelope := ctx.receiveAndVerifyEnvelopes(t, 1, false)[0]
	expectAckUUIDsToContain(t, replyEnvelope, requestEnvelope.UUID)
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
	for _, username := range usernames {
		expectOnlineUser(username)
	}
}
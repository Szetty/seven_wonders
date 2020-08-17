package test

import (
	"fmt"
	"github.com/Szetty/seven_wonders/backend/domain"
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
	server   *httptest.Server
	wsPath   string
	jwt      string
}

func (ctx *wsContext) reconnectWS(t *testing.T) {
	connectWS(t, ctx)
}

func setupWS(t *testing.T, server *httptest.Server, wsPath string) *wsContext {
	username := "user-" + uuid.New().String()
	err, jwt, gameID := server.Config.Handler.(*web.Server).Crux().Auth.CreateUser(username)
	if err != nil {
		t.Fatalf("could not create JWT token: %v", err)
	}
	wsContext := &wsContext{username: username, jwt: jwt, gameID: gameID, server: server, wsPath: wsPath}
	connectWS(t, wsContext)
	return wsContext
}

func connectWS(t *testing.T, ctx *wsContext) {
	baseWSURL := strings.TrimPrefix(ctx.server.URL, "http")
	wsURL := fmt.Sprintf("ws%s%s%s?authorization=%s", baseWSURL, ctx.wsPath, ctx.gameID, ctx.jwt)
	ws, _, err := gWebsocket.DefaultDialer.Dial(wsURL, nil)
	if err != nil {
		t.Fatalf("could not open a WS connection on %s %v", wsURL, err)
	}
	ctx.ws = ws
}

func expectWelcomeMessage(t *testing.T, ws *gWebsocket.Conn) {
	envelopes := receiveAndVerifyEnvelopes(t, ws, 1, false)
	msg := envelopes[0].Data.(domain.Message)
	if msg.MessageType != domain.Welcome {
		t.Fatalf("expected welcome message" + gotAndExpectedMessage(msg.MessageType, domain.UserGotOnline))
	}
}

func envelopeWithMessageType(messageType domain.MessageType) domain.Envelope {
	return domain.
		EnveloperBuilder{}.
		Data(domain.MessageBuilder{}.MessageType(messageType).Build()).
		UUID(uuid.New().String()).
		Build()
}

func envelopeWithMessageTypeAndBody(messageType domain.MessageType, body interface{}) domain.Envelope {
	return domain.
		EnveloperBuilder{}.
		Data(domain.MessageBuilder{}.MessageType(messageType).Body(body).Build()).
		UUID(uuid.New().String()).
		Build()
}

func sendEnvelope(t *testing.T, ws *gWebsocket.Conn, envelope domain.Envelope) {
	err := ws.WriteJSON([]domain.Envelope{envelope})
	if err != nil {
		t.Fatalf("writing to WS failed: %v", err)
	}
}

func receiveAndVerifyEnvelopes(t *testing.T, ws *gWebsocket.Conn, expectedSize int, expectTimeout bool) []domain.Envelope {
	type result struct {
		error     error
		envelopes []domain.Envelope
	}
	ch := make(chan result)
	ticker := time.NewTicker(receiveTimeout)
	go func() {
		err, envelopes := websocket.ReceiveEnvelopes(ws)
		ch <- result{err, envelopes}
	}()
	select {
	case r := <-ch:
		if expectTimeout {
			t.Fatalf("Expected timeout, got: %#v", r)
		}
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
		if !expectTimeout {
			t.Fatalf("Received timeout %s", debug.Stack())
		}
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
		expectedGot := gotAndExpectedMessage(strings.Join(ackUUIDs, ""), uuid)
		t.Fatalf("Expected uuid is not present in ack uuids"+expectedGot+"%s", debug.Stack())
	}
}

func expectMessageType(t *testing.T, message domain.Message, messageType domain.MessageType) {
	if message.MessageType != messageType {
		t.Fatalf("Unexpected message type" + gotAndExpectedMessage(message.MessageType, messageType))
	}
}

func cleanupWS(t *testing.T, ws *gWebsocket.Conn) {
	cm := gWebsocket.FormatCloseMessage(gWebsocket.CloseNormalClosure, "done")
	if err := ws.WriteMessage(gWebsocket.CloseMessage, cm); err != nil {
		t.Fatalf("could not send close message: %v", err)
	}
	_ = ws.Close()
}

func gotAndExpectedMessage(args ...interface{}) string {
	return fmt.Sprintf(", got: %s, expected: %s", args...)
}

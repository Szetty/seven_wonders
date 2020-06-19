package test

import (
	"fmt"
	"github.com/Szetty/seven_wonders/backend/users"
	"github.com/Szetty/seven_wonders/backend/web"
	"github.com/Szetty/seven_wonders/backend/web/websocket"
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
	baseWSURL := strings.TrimPrefix(server.URL, "http")
	err, jwt, gameID := users.CreateUser("test")
	if err != nil {
		t.Fatalf("could not create JWT token: %v", err)
	}
	wsURL := fmt.Sprintf("ws%s/api/secured/game/%s?authorization=%s", baseWSURL, gameID, jwt)
	ws, _, err := gWebsocket.DefaultDialer.Dial(wsURL, nil)
	if err != nil {
		t.Fatalf("could not open a WS connection on %s %v", wsURL, err)
	}
	defer func() {
		cm := gWebsocket.FormatCloseMessage(gWebsocket.CloseNormalClosure, "done")
		if err := ws.WriteMessage(gWebsocket.CloseMessage, cm); err != nil {
			t.Fatalf("could not send close message: %v", err)
		}
		_ = ws.Close()
	}()
	t.Run("receive welcome message", func(t *testing.T) {
		err, envelopes := websocket.ReceiveEnvelopes(ws)
		if err != nil {
			t.Fatalf("could not receive envelopes: %v", err)
		}
		if len(envelopes) != 1 {
			t.Fatalf("got bundle bigger than size 1: %v", envelopes)
		}
		if envelopes[0].UUID == "" {
			t.Fatalf("message has no UUID")
		}
		msg := envelopes[0].Data.(websocket.Message)
		if msg.MessageType != "welcome" {
			t.Fatalf("received message has wrong type: %s", msg.MessageType)
		}
	})
	t.Run("sending message", func(t *testing.T) {
		envelope := websocket.EnveloperBuilder{}.
			Data(
			websocket.MessageBuilder{}.
					MessageType("welcome").
					Body("").
					Build(),
			).
			UUID(uuid.New().String()).
			Build()
		err := ws.WriteJSON([]websocket.Envelope{envelope})
		if err != nil {
			t.Fatalf("writing to WS failed: %v", err)
		}
		time.Sleep(time.Second)
	})
}
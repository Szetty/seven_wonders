package websocket

import (
	"fmt"
	"github.com/Szetty/seven_wonders/backend/common"
	"github.com/Szetty/seven_wonders/backend/dto"
	"github.com/Szetty/seven_wonders/backend/web/errorHandling"
	"github.com/gorilla/websocket"
	"github.com/mitchellh/mapstructure"
	"github.com/pkg/errors"
	"net/http"
)

var upgrader = websocket.Upgrader{}
var logger = common.NewLogger("WebSocket")

func CreateWSSession(w http.ResponseWriter, r *http.Request, username string) *Session {
	wsConn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		errorHandling.ErrorHandler{
			Message:    fmt.Sprintf("Could not upgrade to WS: %v", err),
			StatusCode: 500,
			ErrorType:  errorHandling.ServerError,
		}.ServeHTTP(w, r)
		return nil
	}
	return findOrCreateSession(username, wsConn)
}

func ReceiveEnvelopes(conn *websocket.Conn) (error, []dto.Envelope) {
	var envelopes []dto.Envelope
	err := conn.ReadJSON(&envelopes)
	if err != nil {
		return errors.Wrap(err, "could not read from WS"), envelopes
	}
	var newEnvelopes []dto.Envelope
	for _, envelope := range envelopes {
		var msg dto.Message
		err := mapstructure.Decode(envelope.Data, &msg)
		if err != nil {
			newEnvelopes = append(newEnvelopes, envelope)
			continue
		}
		newEnvelopes = append(newEnvelopes, dto.Envelope{
			Data:     dto.DecodeMessageByType(msg),
			UUID:     envelope.UUID,
			AckUUIDs: envelope.AckUUIDs,
		})
	}
	return nil, newEnvelopes
}

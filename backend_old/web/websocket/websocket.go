package websocket

import (
	"fmt"
	"github.com/Szetty/seven_wonders/backend_old/domain"
	"github.com/Szetty/seven_wonders/backend_old/web/errorHandling"
	"github.com/gorilla/websocket"
	"github.com/mitchellh/mapstructure"
	"github.com/pkg/errors"
	"net/http"
)

var upgrader = websocket.Upgrader{}

func CreateWSSession(w http.ResponseWriter, r *http.Request, username string, sessions *Sessions) (*Session, bool) {
	wsConn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		errorHandling.ErrorHandler{
			Message:    fmt.Sprintf("Could not upgrade to WS: %v", err),
			StatusCode: 500,
			ErrorType:  errorHandling.ServerError,
		}.ServeHTTP(w, r)
		return nil, false
	}
	return sessions.findOrCreateSession(username, wsConn)
}

func ReceiveEnvelopes(conn *websocket.Conn) (error, []domain.Envelope) {
	var envelopes []domain.Envelope
	err := conn.ReadJSON(&envelopes)
	if err != nil {
		return errors.Wrap(err, "could not read from WS"), envelopes
	}
	var newEnvelopes []domain.Envelope
	for _, envelope := range envelopes {
		var msg domain.Message
		err := mapstructure.Decode(envelope.Data, &msg)
		if err != nil {
			newEnvelopes = append(newEnvelopes, envelope)
			continue
		}
		newEnvelopes = append(newEnvelopes, domain.Envelope{
			Data:     domain.DecodeMessage(msg),
			UUID:     envelope.UUID,
			AckUUIDs: envelope.AckUUIDs,
		})
	}
	return nil, newEnvelopes
}

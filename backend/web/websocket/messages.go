package websocket

import (
	"github.com/gorilla/websocket"
	"github.com/mitchellh/mapstructure"
	"github.com/pkg/errors"
)

type Envelope struct {
	Data     interface{} `json:"data"`
	UUID     string      `json:"uuid"`
	AckUUIDs []string    `json:"ack_uuids"`
}

type Message struct {
	MessageType string      `json:"type" mapstructure:"type"`
	Body        interface{} `json:"body" mapstructure:"body"`
}

type ErrorBody struct {
	Code string      `json:"code"`
	Info interface{} `json:"info"`
}

func ReceiveEnvelopes(conn *websocket.Conn) (error, []Envelope) {
	var envelopes []Envelope
	err := conn.ReadJSON(&envelopes)
	if err != nil {
		return errors.Wrap(err, "could not read from WS"), envelopes
	}
	var newEnvelopes []Envelope
	for _, envelope := range envelopes {
		var msg Message
		err := mapstructure.Decode(envelope.Data, &msg)
		if err != nil {
			return nil, envelopes
		}
		newEnvelopes = append(newEnvelopes, Envelope{
			Data:     msg,
			UUID:     envelope.UUID,
			AckUUIDs: envelope.AckUUIDs,
		})
	}
	return nil, newEnvelopes
}
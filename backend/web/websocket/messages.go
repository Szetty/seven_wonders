package websocket

type Bundle = []Envelope
type Envelope struct {
	Data interface{} `json:"data"`
	UUID string `json:"uuid"`
	AckUUIDs []string `json:"ack_uuids"`
}
type Message struct {
	MessageType string `json:"type"`
	Body interface{} `json:"body"`
}

type ErrorBody struct {
	Code string `json:"code"`
	Info interface{} `json:"info"`
}

func welcomeFromServerMessage(sessionKey string) Message {
	return Message{
		MessageType: "welcome",
		Body:        sessionKey,
	}
}
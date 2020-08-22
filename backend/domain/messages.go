package domain

type MessageType string

const Welcome = "Welcome"

type OriginEnvelope struct {
	Envelope
	Origin
}

type Envelope struct {
	Data     interface{} `json:"data"`
	UUID     string      `json:"uuid"`
	AckUUIDs []string    `json:"ack_uuids"`
}

type Message struct {
	MessageType MessageType `json:"type" mapstructure:"type"`
	Body        interface{} `json:"body" mapstructure:"body"`
}

type ErrorBody struct {
	Code string      `json:"code"`
	Info interface{} `json:"info"`
}

var decoders = []func(Message) Message{
	decodeLobbyMessage,
	decodeUsersMessage,
}

func DecodeMessage(message Message) Message {
	newMessage := message
	for _, decoder := range decoders {
		newMessage = decoder(newMessage)
	}
	return newMessage
}

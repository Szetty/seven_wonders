package domain

type MessageType string

const Welcome MessageType = "Welcome"

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

var decoders = []func(Message) Message{
	decodeLobbyMessage,
	decodeUsersMessage,
	errorDecoder,
}

func DecodeMessage(message Message) Message {
	for _, decoder := range decoders {
		message = decoder(message)
	}
	return message
}

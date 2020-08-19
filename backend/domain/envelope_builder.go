package domain

import "github.com/google/uuid"

type EnveloperBuilder struct {
	data     interface{}
	uuid     string
	ackUUIDs []string
}

func (b EnveloperBuilder) Data(data interface{}) EnveloperBuilder {
	b.data = data
	return b
}

func (b EnveloperBuilder) UUID(uuid string) EnveloperBuilder {
	b.uuid = uuid
	return b
}

func (b EnveloperBuilder) GenerateUUID() EnveloperBuilder {
	b.uuid = uuid.New().String()
	return b
}

func (b EnveloperBuilder) AckUUIDs(ackUUIDs []string) EnveloperBuilder {
	b.ackUUIDs = ackUUIDs
	return b
}

func (b EnveloperBuilder) Ack(ackUUIDs []string) Envelope {
	b.ackUUIDs = ackUUIDs
	b.data = "ack"
	return b.Build()
}

func (b EnveloperBuilder) Build() Envelope {
	return Envelope{
		Data:     b.data,
		UUID:     b.uuid,
		AckUUIDs: b.ackUUIDs,
	}
}

type MessageBuilder struct {
	messageType MessageType
	body        interface{}
}

func (b MessageBuilder) MessageType(messageType MessageType) MessageBuilder {
	b.messageType = messageType
	return b
}

func (b MessageBuilder) Body(body interface{}) MessageBuilder {
	b.body = body
	return b
}

func (b MessageBuilder) Error(code string, info interface{}) Message {
	b.messageType = "error"
	b.body = ErrorBody{
		Code: code,
		Info: info,
	}
	return b.Build()
}

func (b MessageBuilder) Build() Message {
	return Message{
		MessageType: b.messageType,
		Body:        b.body,
	}
}

func NewOrigin(id string, originType OriginType) Origin {
	return Origin{ID: id, OriginType: originType}
}

func (e Envelope) WithOrigin(origin Origin) OriginEnvelope {
	return OriginEnvelope{Envelope: e, Origin: origin}
}

func Reply(envelope Envelope, message Message) Envelope {
	return EnveloperBuilder{}.AckUUIDs([]string{envelope.UUID}).Data(message).Build()
}

func Notify(message Message) Envelope {
	return EnveloperBuilder{}.GenerateUUID().Data(message).Build()
}

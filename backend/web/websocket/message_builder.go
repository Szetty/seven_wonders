package websocket

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
	messageType string
	body        interface{}
}

func (b MessageBuilder) MessageType(messageType string) MessageBuilder {
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

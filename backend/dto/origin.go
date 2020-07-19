package dto

import "github.com/Szetty/seven_wonders/backend/logger"

type OriginType string

const (
	Empty       OriginType = "Empty"
	FromSession            = "FromSession"
	FromLobby              = "FromLobby"
	FromUsers              = "FromUsers"
)

type Origin struct {
	ID         string
	OriginType OriginType
}

func EmptyOrigin() Origin {
	return Origin{OriginType: Empty}
}

func ReplyToOrigin(ch chan<- OriginEnvelope, envelope OriginEnvelope, replyMessage Message, origin Origin) {
	switch envelope.OriginType {
	case FromSession:
		ch <- Reply(envelope.Envelope, replyMessage).WithOrigin(origin)
	default:
		logger.L.Warnf("Unknown origin type: %s", envelope.OriginType)
	}
}

func NotifyTarget(ch chan<- OriginEnvelope, message Message, origin Origin) {
	ch <- Notify(message).WithOrigin(origin)
}

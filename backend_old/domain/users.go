package domain

const (
	// Request
	OnlineUsers MessageType = "OnlineUsers"
	// Reply
	OnlineUsersReply MessageType = "OnlineUsersReply"
	// Notifications
	GotOnline  MessageType = "UserGotOnline"
	GotOffline MessageType = "UserGotOffline"
)

func decodeUsersMessage(message Message) Message {
	switch message.MessageType {
	case OnlineUsers:
		message.Body = nil
	case OnlineUsersReply:
		var onlineUsers []string
		for _, onlineUser := range message.Body.([]interface{}) {
			onlineUsers = append(onlineUsers, onlineUser.(string))
		}
		message.Body = onlineUsers
	case GotOnline:
		message.Body = message.Body.(string)
	case GotOffline:
		message.Body = message.Body.(string)
	default:
		break
	}
	return message
}

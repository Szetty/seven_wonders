package domain

import (
	"github.com/Szetty/seven_wonders/backend/logger"
	"github.com/mitchellh/mapstructure"
	"go/types"
)

const (
	// Request
	OnlineUsers  MessageType = "OnlineUsers"
	InvitedUsers MessageType = "InvitedUsers"
	InviteUser   MessageType = "InviteUser"
	UninviteUser MessageType = "UninviteUser"
	// Reply
	OnlineUsersReply  MessageType = "OnlineUsersReply"
	InvitedUsersReply MessageType = "InvitedUsersReply"
	InviteUserReply   MessageType = "InviteUserReply"
	UninviteUserReply MessageType = "UninviteUserReply"
	// Notifications
	GotInvite      MessageType = "GotInvite"
	GotUninvite    MessageType = "GotUninvite"
	UserGotOnline  MessageType = "UserGotOnline"
	UserGotOffline MessageType = "UserGotOffline"
	StartGame      MessageType = "StartGame"
)

type InvitedUser struct {
	Name      string `json:"name" mapstructure:"name"`
	Connected bool   `json:"connected" mapstructure:"connected"`
}

type User struct {
	Name   string `json:"name" mapstructure:"name"`
	GameID string `json:"gameID" mapstructure:"gameID"`
}

func DecodeMessageByType(message Message) Message {
	switch message.MessageType {
	case OnlineUsers:
		message.Body = nil
	case OnlineUsersReply:
		var onlineUsers []string
		for _, onlineUser := range message.Body.([]interface{}) {
			onlineUsers = append(onlineUsers, onlineUser.(string))
		}
		message.Body = onlineUsers
	case InviteUser:
		message.Body = message.Body.(string)
	case InviteUserReply:
		message.Body = nil
	case InvitedUsers:
		message.Body = nil
	case InvitedUsersReply:
		var invitedUsers []InvitedUser
		switch body := message.Body.(type) {
		case types.Nil:
			message.Body = []InvitedUser{}
		case []interface{}:
			for _, invitedUserMap := range body {
				var invitedUser InvitedUser
				err := mapstructure.Decode(invitedUserMap, &invitedUser)
				if err != nil {
					logger.L.Errorf("Could not decode invitedUserMap %#v, because: %v", invitedUserMap, err)
					continue
				}
				invitedUsers = append(invitedUsers, invitedUser)
			}
		}
		message.Body = invitedUsers
	case UninviteUser:
		message.Body = message.Body.(string)
	case UninviteUserReply:
		message.Body = nil
	case GotInvite:
		var user User
		switch body := message.Body.(type) {
		case map[string]interface{}:
			err := mapstructure.Decode(body, &user)
			if err != nil {
				logger.L.Errorf("Could not decode user body map %#v, because: %v", body, err)
			}
		}
		message.Body = user
	case GotUninvite:
		message.Body = message.Body.(string)
	case UserGotOnline:
		message.Body = message.Body.(string)
	case UserGotOffline:
		message.Body = message.Body.(string)
	default:
		break
	}
	return message
}

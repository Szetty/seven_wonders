package dto

import (
	"github.com/mitchellh/mapstructure"
	"go/types"
)

const (
	// Request
	OnlineUsers       MessageType = "OnlineUsers"
	InvitedUsers                  = "InvitedUsers"
	InviteUser                    = "InviteUser"
	UninviteUser                  = "UninviteUser"
	// Reply
	OnlineUsersReply              = "OnlineUsersReply"
	InvitedUsersReply             = "InvitedUsersReply"
	InviteUserReply               = "InviteUserReply"
	UninviteUserReply             = "UninviteUserReply"
	// Notifications
	GotInvite                     = "GotInvite"
	GotUninvite                   = "GotUninvite"
	StartGame                     = "StartGame"
)

type InvitedUser struct {
	Name      string `json:"name" mapstructure:"name"`
	Connected bool   `json:"connected" mapstructure:"connected"`
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
					logger.Errorf("Could not decode invitedUserMap %#v, because: %v", invitedUserMap, err)
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
		message.Body = message.Body.(string)
	case GotUninvite:
		message.Body = message.Body.(string)
	default:
		break
	}
	return message
}

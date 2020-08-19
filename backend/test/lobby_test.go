package test

import (
	"github.com/Szetty/seven_wonders/backend/domain"
	"github.com/Szetty/seven_wonders/backend/web"
	"net/http/httptest"
	"testing"
)

func TestInviteUsers(t *testing.T) {
	server := httptest.NewServer(web.MainHandler())
	defer server.Close()
	ctx1 := setupWS(t, server, "/api/secured/game/lobby/")
	ctx2 := setupWS(t, server, "/api/secured/game/lobby/")
	defer cleanupWS(t, ctx1.ws)
	expectWelcomeMessage(t, ctx1.ws)
	expectWelcomeMessage(t, ctx2.ws)
	expectUserGotOnline(t, ctx1.ws, ctx2.username)
	invitedUsersRequest1 := envelopeWithMessageType(domain.InvitedUsers)
	sendEnvelope(t, ctx1.ws, invitedUsersRequest1)
	invitedUsersReply1 := receiveAndVerifyEnvelopes(t, ctx1.ws, 1, false)[0]
	expectAckUUIDsToContain(t, invitedUsersReply1.AckUUIDs, invitedUsersRequest1.UUID)
	invitedUsersMessage1 := invitedUsersReply1.Data.(domain.Message)
	expectMessageType(t, invitedUsersMessage1, domain.InvitedUsersReply)
	invitedUsers1 := invitedUsersMessage1.Body.([]domain.InvitedUser)
	if len(invitedUsers1) != 1 {
		t.Fatalf("Expecting only leader to be invited user")
	}
	if invitedUsers1[0].Name != ctx1.username {
		t.Fatalf("Wrong invited user")
	}
	if !invitedUsers1[0].Leader {
		t.Fatalf("The only invited user should be the leader")
	}
	if !invitedUsers1[0].Connected {
		t.Fatalf("The leader should already be connected")
	}

	inviteUserRequest := envelopeWithMessageTypeAndBody(domain.InviteUser, ctx2.username)
	sendEnvelope(t, ctx1.ws, inviteUserRequest)
	inviteUserReply := receiveAndVerifyEnvelopes(t, ctx1.ws, 1, false)[0]
	expectAckUUIDsToContain(t, inviteUserReply.AckUUIDs, inviteUserRequest.UUID)
	inviteUserMessage := inviteUserReply.Data.(domain.Message)
	expectMessageType(t, inviteUserMessage, domain.InviteUserReply)

	invitedUsersRequest2 := envelopeWithMessageType(domain.InvitedUsers)
	sendEnvelope(t, ctx1.ws, invitedUsersRequest2)
	invitedUsersReply2 := receiveAndVerifyEnvelopes(t, ctx1.ws, 1, false)[0]
	expectAckUUIDsToContain(t, invitedUsersReply2.AckUUIDs, invitedUsersRequest2.UUID)
	invitedUsersMessage2 := invitedUsersReply2.Data.(domain.Message)
	expectMessageType(t, invitedUsersMessage2, domain.InvitedUsersReply)
	invitedUsers2 := invitedUsersMessage2.Body.([]domain.InvitedUser)
	if len(invitedUsers2) != 2 {
		t.Fatalf("Expecting two invited users")
	}
	invitedUsers2 = removeLeaderFromInvitedUsers(invitedUsers2)
	if invitedUsers2[0].Name != ctx2.username {
		t.Fatalf("Wrong invited user" + gotAndExpectedMessage(invitedUsers2[0].Name, ctx2.username))
	}

	gotInviteEnvelope := receiveAndVerifyEnvelopes(t, ctx2.ws, 1, false)[0]
	gotInviteMessage := gotInviteEnvelope.Data.(domain.Message)
	expectMessageType(t, gotInviteMessage, domain.GotInvite)
	inviter := gotInviteMessage.Body.(domain.User)
	if inviter.GameID != ctx1.gameID {
		t.Fatalf("Wrong invited game id" + gotAndExpectedMessage(inviter.GameID, ctx1.gameID))
	}
	if inviter.Name != ctx1.username {
		t.Fatalf("Wrong inviter name" + gotAndExpectedMessage(inviter.Name, ctx1.username))
	}
}

func removeLeaderFromInvitedUsers(invitedUsers []domain.InvitedUser) []domain.InvitedUser {
	var newInvitedUsers []domain.InvitedUser
	for _, user := range invitedUsers {
		if !user.Leader {
			newInvitedUsers = append(newInvitedUsers, user)
		}
	}
	return newInvitedUsers
}

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
	defer ctx1.close(t)
	defer ctx2.close(t)
	ctx1.expectWelcomeMessage(t)
	ctx2.expectWelcomeMessage(t)
	ctx1.expectUserGotOnline(t, ctx2.username)

	testLeaderIsInvited(t, ctx1)

	sendInviteUser(t, ctx1, ctx2.username)

	invitedUsersRequest2 := envelopeWithMessageType(domain.InvitedUsers)
	ctx1.sendEnvelope(t, invitedUsersRequest2)
	invitedUsersReply2 := ctx1.receiveAndVerifyEnvelopes(t, 1, false)[0]
	expectAckUUIDsToContain(t, invitedUsersReply2, invitedUsersRequest2.UUID)
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

	expectGotInvite(t, ctx2, domain.User{Name: ctx1.username, GameID: ctx1.gameID})
}

func TestAcceptInvitation(t *testing.T) {
	server := httptest.NewServer(web.MainHandler())
	defer server.Close()
	ctx1 := setupWS(t, server, "/api/secured/game/lobby/")
	ctx2 := setupWS(t, server, "/api/secured/game/lobby/")
	defer ctx1.close(t)
	ctx1.expectWelcomeMessage(t)
	ctx2.expectWelcomeMessage(t)
	ctx1.expectUserGotOnline(t, ctx2.username)

	sendInviteUser(t, ctx1, ctx2.username)
	expectGotInvite(t, ctx2, domain.User{Name: ctx1.username, GameID: ctx1.gameID})

	ctx2.close(t)
	ctx2.connectWSTo(t, ctx1.gameID)
	defer ctx2.close(t)
	ctx2.expectWelcomeMessage(t)
	acceptedInvitationEnvelope := ctx1.receiveAndVerifyEnvelopes(t, 1, false)[0]
	acceptedInvitationMessage := acceptedInvitationEnvelope.Data.(domain.Message)
	expectMessageType(t, acceptedInvitationMessage, domain.Connected)
	username := acceptedInvitationMessage.Body.(string)
	if username != ctx2.username {
		t.Fatalf("Wrong accepted invitation username" + gotAndExpectedMessage(username, ctx2.username))
	}
}

func TestDeclineInvitation(t *testing.T) {
	server := httptest.NewServer(web.MainHandler())
	defer server.Close()
	ctx1 := setupWS(t, server, "/api/secured/game/lobby/")
	ctx2 := setupWS(t, server, "/api/secured/game/lobby/")
	defer ctx1.close(t)
	ctx1.expectWelcomeMessage(t)
	ctx2.expectWelcomeMessage(t)
	ctx1.expectUserGotOnline(t, ctx2.username)

	sendInviteUser(t, ctx1, ctx2.username)
	expectGotInvite(t, ctx2, domain.User{Name: ctx1.username, GameID: ctx1.gameID})

	declineInvitationRequest := envelopeWithMessageTypeAndBody(domain.DeclineInvitation, ctx1.gameID)
	ctx2.sendEnvelope(t, declineInvitationRequest)
	declineInvitationReply := ctx2.receiveAndVerifyEnvelopes(t, 1, false)[0]
	expectAckUUIDsToContain(t, declineInvitationReply, declineInvitationRequest.UUID)
	declineInvitationReplyMessage := declineInvitationReply.Data.(domain.Message)
	expectMessageType(t, declineInvitationReplyMessage, domain.DeclineInvitationReply)

	declinedInvitation := ctx1.receiveAndVerifyEnvelopes(t, 1, false)[0]
	declinedInvitationMessage := declinedInvitation.Data.(domain.Message)
	expectMessageType(t, declinedInvitationMessage, domain.DeclinedInvitation)
	declinedUsername := declinedInvitationMessage.Body.(string)
	if declinedUsername != ctx2.username {
		t.Fatalf("Wrong declined invitation username" + gotAndExpectedMessage(declinedUsername, ctx2.username))
	}
}

func TestUnauthorizedInvite(t *testing.T) {
	server := httptest.NewServer(web.MainHandler())
	defer server.Close()
	ctx1 := setupWS(t, server, "/api/secured/game/lobby/")
	ctx2 := setupWS(t, server, "/api/secured/game/lobby/")
	defer ctx1.close(t)
	ctx1.expectWelcomeMessage(t)
	ctx2.expectWelcomeMessage(t)
	ctx1.expectUserGotOnline(t, ctx2.username)

	sendInviteUser(t, ctx1, ctx2.username)
	expectGotInvite(t, ctx2, domain.User{Name: ctx1.username, GameID: ctx1.gameID})

	ctx2.close(t)
	ctx2.connectWSTo(t, ctx1.gameID)
	defer ctx2.close(t)
	ctx2.expectWelcomeMessage(t)

	inviteUserRequest := envelopeWithMessageTypeAndBody(domain.InviteUser, ctx1.username)
	ctx2.sendEnvelope(t, inviteUserRequest)
	inviteUserReply := ctx2.receiveAndVerifyEnvelopes(t, 1, false)[0]
	expectAckUUIDsToContain(t, inviteUserReply, inviteUserRequest.UUID)
	inviteUserMessage := inviteUserReply.Data.(domain.Message)
	expectMessageType(t, inviteUserMessage, domain.ErrorMessageType)
	errorBody := inviteUserMessage.Body.(domain.ErrorBody)
	expectedErrorCode := domain.Unauthorized
	if errorBody.Code != expectedErrorCode {
		t.Fatalf("Wrong error code" + gotAndExpectedMessage(errorBody.Code, expectedErrorCode))
	}
}

func TestReturnToOwnLobby(t *testing.T) {
	server := httptest.NewServer(web.MainHandler())
	defer server.Close()
	ctx1 := setupWS(t, server, "/api/secured/game/lobby/")
	ctx2 := setupWS(t, server, "/api/secured/game/lobby/")
	defer ctx1.close(t)
	ctx1.expectWelcomeMessage(t)
	ctx2.expectWelcomeMessage(t)
	ctx1.expectUserGotOnline(t, ctx2.username)

	sendInviteUser(t, ctx1, ctx2.username)
	expectGotInvite(t, ctx2, domain.User{Name: ctx1.username, GameID: ctx1.gameID})

	ctx2.close(t)
	ctx2.connectWSTo(t, ctx1.gameID)
	ctx2.expectWelcomeMessage(t)

	ctx2.close(t)
	ctx2.reconnectWS(t)
	ctx2.expectWelcomeMessage(t)
	testOnlineUsers(t, ctx2, []string{ctx1.username, ctx2.username})
	testLeaderIsInvited(t, ctx2)
}

func testLeaderIsInvited(t *testing.T, ctx *wsContext) {
	invitedUsersRequest := envelopeWithMessageType(domain.InvitedUsers)
	ctx.sendEnvelope(t, invitedUsersRequest)
	invitedUsersReply := ctx.receiveAndVerifyEnvelopes(t, 1, false)[0]
	expectAckUUIDsToContain(t, invitedUsersReply, invitedUsersRequest.UUID)
	invitedUsersMessage := invitedUsersReply.Data.(domain.Message)
	expectMessageType(t, invitedUsersMessage, domain.InvitedUsersReply)
	invitedUsers := invitedUsersMessage.Body.([]domain.InvitedUser)
	if len(invitedUsers) != 1 {
		t.Fatalf("Expecting only leader to be invited user")
	}
	if invitedUsers[0].Name != ctx.username {
		t.Fatalf("Wrong invited user")
	}
	if !invitedUsers[0].Leader {
		t.Fatalf("The only invited user should be the leader")
	}
	if !invitedUsers[0].Connected {
		t.Fatalf("The leader should already be connected")
	}
}

func sendInviteUser(t *testing.T, ctx *wsContext, invitedUserName string) {
	inviteUserRequest := envelopeWithMessageTypeAndBody(domain.InviteUser, invitedUserName)
	ctx.sendEnvelope(t, inviteUserRequest)
	inviteUserReply := ctx.receiveAndVerifyEnvelopes(t, 1, false)[0]
	expectAckUUIDsToContain(t, inviteUserReply, inviteUserRequest.UUID)
	inviteUserMessage := inviteUserReply.Data.(domain.Message)
	expectMessageType(t, inviteUserMessage, domain.InviteUserReply)
}

func expectGotInvite(t *testing.T, ctx *wsContext, expectedInviter domain.User) {
	gotInviteEnvelope := ctx.receiveAndVerifyEnvelopes(t, 1, false)[0]
	gotInviteMessage := gotInviteEnvelope.Data.(domain.Message)
	expectMessageType(t, gotInviteMessage, domain.GotInvite)
	inviter := gotInviteMessage.Body.(domain.User)
	if inviter != expectedInviter {
		t.Fatalf("Wrong inviter" + gotAndExpectedMessage(inviter, expectedInviter))
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

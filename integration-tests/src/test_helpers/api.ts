export type Username = string;
export type User = {
  name: Username,
  gameID: string
}
export type InvitedUser = {
  connected: boolean,
  leader: boolean,
  name: string
}
export type InviteUser = string
export type InviteUserReply = {} | ErrorBody
export type AcceptInvitationReply = {}
export type DeclineInvitationReply = {}
export type ErrorBody = {
  code: string,
  info: any
}
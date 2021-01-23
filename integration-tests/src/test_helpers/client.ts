import { AcceptInvitationReply, DeclineInvitationReply, InvitedUser, InviteUserReply, User, Username } from "./api";
import { WSWrapper } from "./ws_wrapper";

export interface UserContext {
  name: string
  userToken: string
  gameID: string
}

export class Client {
  public username: Username;
  public gameID: string;
  private wsWrapper: WSWrapper;

  constructor(private baseURL: string, private userContext: UserContext) {
    const fullURL = this.buildWSURL(this.userContext.gameID);
    this.wsWrapper = new WSWrapper(fullURL);
    this.username = this.userContext.name;
    this.gameID = this.userContext.gameID;
  }

  // Request-Reply messages
  async getOnlineUsers(): Promise<Username[]> {
    return await this.wsWrapper.call<null, Username[]>('OnlineUsers', null);
  }
  async getInvitedUsers(): Promise<InvitedUser[]> {
    return await this.wsWrapper.call<null, InvitedUser[]>('InvitedUsers', null);
  }
  async inviteUser(user: Username): Promise<InviteUserReply> {
    return await this.wsWrapper.call<Username, InviteUserReply>('InviteUser', user);
  }
  async acceptInvitation(gameID: string): Promise<AcceptInvitationReply> {
    this.wsWrapper.close();
    this.wsWrapper = new WSWrapper(this.buildWSURL(gameID));
    await this.receiveSyncData();
    return {} as AcceptInvitationReply;
  }
  async declineInvitation(gameID: string) {
    return await this.wsWrapper.call<string, DeclineInvitationReply>('DeclineInvitation', gameID);
  }
  async returnToOwnLobby() {
    this.wsWrapper.close();
    this.wsWrapper = new WSWrapper(this.buildWSURL(this.userContext.gameID));
    await this.receiveSyncData();
  }

  // Notifications
  async onOnlineUsers(): Promise<Username[]> {
    return await this.wsWrapper.receive<Username>('UserGotOnline');
  }
  async onOfflineUsers(): Promise<Username[]> {
    return await this.wsWrapper.receive<Username>('UserGotOffline');
  }
  async onInvite(): Promise<User[]> {
    return await this.wsWrapper.receive<User>('GotInvite');
  }
  async onConnected(): Promise<Username[]> {
    return await this.wsWrapper.receive<Username>('Connected');
  }
  async onDeclineInvitation(): Promise<Username[]> {
    return await this.wsWrapper.receive<Username>('DeclinedInvitation');
  }
  
  close = () => this.wsWrapper.close()
  reconnect = () => this.wsWrapper.reconnect()
  receiveSyncData = () => this.wsWrapper.receiveSyncData()

  private buildWSURL(gameID: string) {
    return `${this.baseURL}/${gameID}?authorization=${this.userContext.userToken}`;
  }
}
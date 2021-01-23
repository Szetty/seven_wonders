import { initWebSocket, WebSocketService, MessageFromServer } from "websocket-client";
import { delay } from "./common";

export class WSWrapper {
  private webSocketService: WebSocketService;
  private incomingMessages: Record<string, any>;
  private syncData = [];

  constructor(
    url: string,
    onOnline: () => void = () => { },
    onOffline: () => void = () => { },
    onError: (DataError) => void = () => { }
  ) {
    this.incomingMessages = {};
    this.webSocketService = initWebSocket(url, {
      onSync: (data) => {
        this.syncData.push(data);
      },
      onOnline,
      onOffline,
      onIncomingMessage: (data: MessageFromServer) => {
        if (!this.incomingMessages[data.type]) {
          this.incomingMessages[data.type] = [];
        }
        this.incomingMessages[data.type].push(data.body);
      },
      onError,
    });
  }

  public async call<Req, Resp>(type: string, body: Req): Promise<Resp> {
    let res = await this.webSocketService.sendMessage({ type, body });
    return res.body as Resp;
  }

  public async receive<T>(type: string): Promise<T[]> {
    let retries = 10;
    while (retries > 0 && (!this.incomingMessages[type] || this.incomingMessages[type].length == 0)) {
      await delay(100);
      retries--;
    }
    if (retries == 0) {
      return [];
    }
    const res = this.incomingMessages[type];
    this.incomingMessages[type] = [];
    return res as T[];
  }

  public async receiveSyncData(): Promise<any[]> {
    let retries = 10;
    while (retries > 0 && (!this.syncData || this.syncData.length == 0)) {
      await delay(100);
      retries--;
    }
    if (retries == 0) {
      return [];
    }
    const res = this.syncData;
    this.syncData = [];
    return res;
  }

  public close() {
    this.webSocketService.close();
  }

  public reconnect() {
    this.incomingMessages = {};
    this.syncData = [];
    this.webSocketService.close();
    this.webSocketService.forceReconnect();
  }
}
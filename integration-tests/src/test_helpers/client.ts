import { initWebSocket, WebSocketService, MessageFromServer, WelcomeFromServer } from "websocket-client";
import { delay } from "./common";

export interface UserContext {
    name: string
    userToken: string
    gameID: string
}

export class Client {
    private webSocketService: WebSocketService;
    private incomingMessages: Record<string, any> = [];
    private userContext: UserContext;

    constructor(url: string, userContext: UserContext, onSync: (data: WelcomeFromServer) => void = () => {}, onOnline: () => void = () => {}, onOffline: () => void = () => {}) {
        this.userContext = userContext;
        const fullURL = `${url}/${userContext.gameID}?authorization=${this.userContext.userToken}`;
        this.webSocketService = initWebSocket(fullURL, {
            onSync,
            onOnline,
            onOffline,
            onIncomingMessage(data: MessageFromServer) { 
                if (!this.incomingMessages[data.type]) {
                    this.incomingMessages[data.type] = [];
                }
                this.incomingMessages[data.type].push(data.body);
            }
        });
    }

    public async call<Req, Resp>(type: string, body: Req): Promise<Resp> {
        let res = await this.webSocketService.sendMessage({type, body});
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

    public close() {
        this.webSocketService.close();
    }
}
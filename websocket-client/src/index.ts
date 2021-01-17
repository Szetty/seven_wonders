import * as interfaces from "./interfaces";
import { WebSocketService } from "./WebSocketService";

export function initWebSocket(wsUrl: string, webSocketProxy: interfaces.WebSocketProxy): interfaces.WebSocketService {
    return new WebSocketService(wsUrl, webSocketProxy);
}

export * from './interfaces';
export * from './data';
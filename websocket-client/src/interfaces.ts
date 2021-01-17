import * as data from './data';

export type RetryPolicy = "never" | "same_session" | "any_session";

export type DiagnosticsData = {
    ack_elapsed?: number; // time request took on server
    now: number;
    ts: number; // `now-ts` gives you time request took from start to finish, it might inlcude time connection was offline
    uuid: string; // uuid including tag if it was passed
    data: data.MessageReqToServer;
    reject_reason: undefined | data.ErrorFromServer | { type: "fatal_error"; reason: any }; // `undefined` for resolved requests
    retry_policy: RetryPolicy;
    timed_out_at_ts: number; // `timed_out_at_ts-ts` gives you timeout request was sent with
};

export interface MessageOptions {
    retry_policy?: RetryPolicy;
    timeout_milliseconds?: number;
    tag?: string;
    diagnostics_callback?: (diagnostics: DiagnosticsData) => void; // you can rely that it will be called before promise will be resolved or rejected
    uuid?: string;
}

export interface DebugOptions {
    wsLatency?: number;
    wsSendDelay?: number;
    wsReceiveDelay?: number;
}

export type ErrorType =
    | "websocket_error"
    | "websocket_fatal_error"
    | "reconnecting_error"
    | "incoming_message_unknown_type"
    | "incoming_message_uuid_missing"
    | "going_offline"
    | "info_ping_outdated_response"
    | "info_ping_failed"
    | "info_ping"
    | "info_set_session_token"
    | "info_update_session_token"
    | "info_wrong_session_token"
    | "deprecated"
    | "other";

export interface DataError {
    type: ErrorType;
    error: Error;
    info: any;
}

export interface WebSocketService {
    setProxy(proxy: WebSocketProxy): void;
    timeBeforeNextReconnectMs(): number;
    resetReconnectTimer(): void;
    forceReconnect(): void;
    setDebugOptions(options: DebugOptions): void;
    bundle<T>(fn: () => T): void;
    sendMessage(message: data.MessageReqToServer): Promise<data.MessageFromServer>;
    close(): void;
}

export interface WebSocketProxy {
    onError?(error: DataError): void;
    onDiagnostics?(diagnostics: DiagnosticsData): void;
    onSync(data: data.WelcomeFromServer): void;
    onOnline(): void;
    onOffline(): void;
    onIncomingMessage(data: data.MessageFromServer): void;
}
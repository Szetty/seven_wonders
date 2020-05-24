// TO SERVER

export type BundleToServer = EnvelopeToServer[];
export type EnvelopeToServer = EnvelopeReqToServer | EnvelopeAckToServer;

export interface EnvelopeReqToServer {
    uuid: string;
    data: MessageReqToServer;
}

export type MessageReqToServer =
    | PingToServer
    | MessageToServer

export type PingToServer = "ping";
export interface MessageToServer {
    type: string;
    body: any;
}

export interface EnvelopeAckToServer {
    ack_uuids: string[];
    data: MessageAckToServer;
}

export type MessageAckToServer = AckToServer;
export type AckToServer = "ack";


// FROM SERVER

export type BundleFromServer = EnvelopeFromServer[];
export type EnvelopeFromServer = EnvelopeReqFromServer | EnvelopeAckFromServer;

export interface EnvelopeReqFromServer {
    uuid: string;
    data: MessageReqFromServer;
}

export type MessageReqFromServer =
    | WelcomeFromServer
    | MessageFromServer

export interface WelcomeFromServer {
    type: "welcome";
    session_key: string;
}

export interface MessageFromServer {
    type: string;
    body: any;
}

export interface EnvelopeAckFromServer {
    ack_elapsed?: { [key: string]: number };
    ack_uuids: string[];
    data: MessageAckFromServer;
}

export type MessageAckFromServer =
    | MessageReqFromServer
    | AckFromServer
    | ErrorFromServer

export type AckFromServer = "ack";

export interface ErrorFromServer {
    type: "error";
    body: {
        code: string;
        info?: any;
    }
}
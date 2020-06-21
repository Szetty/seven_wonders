enum WebSocketServiceStatus {
    Initial,
    Connecting,
    Open,
    ShuttingDown,
    Close,
}

export class WebSocketConnection {
    constructor(
        private _webSocketClass: typeof WebSocket | undefined,
        private _url: string,
        private _handlers: {
            onMessage: (data: any) => void;
            onConnected: () => void;
            onClose: (ev: CloseEvent) => void;
            onError: (error: Error) => void;
        } | null
    ) {
        if (typeof this._webSocketClass !== "function") {
            if (this._handlers) {
                this._handlers.onError(
                    new Error("WebSocketClass is not a function. Will not connect to server.")
                );
            }
            return;
        }
        this.openws();
    }

    private ws: WebSocket | null;

    private status = WebSocketServiceStatus.Initial;

    private createdTs = new Date().getTime();

    public get uptime() {
        return new Date().getTime() - this.createdTs;
    }

    public close() {
        this.shutdown();
    }

    openws = () => {
        if (!this._webSocketClass) {
            return;
        }
        this.status = WebSocketServiceStatus.Connecting;
        if (this.ws) {
            this.ws.removeEventListener("open", this.handleOpen);
            this.ws.removeEventListener("error", this.handleError);
            this.ws.removeEventListener("close", this.handleClose);
            this.ws.removeEventListener("message", this.handleMessage);
            this.ws = null;
        }

        this.ws = new this._webSocketClass(this._url);
        this.ws.addEventListener("open", this.handleOpen);
        this.ws.addEventListener("error", this.handleError);
        this.ws.addEventListener("close", this.handleClose);
        this.ws.addEventListener("message", this.handleMessage);
    };
    shutdown = () => {
        this._handlers = null;
        this.status = WebSocketServiceStatus.ShuttingDown;
        if (this.ws) {
            this.ws.close();
        }
    };
    handleOpen = () => {
        if (this._handlers) {
            this.status = WebSocketServiceStatus.Open;
            this._handlers.onConnected();
        } else {
            this.shutdown();
        }
    };
    handleError = (_ev: ErrorEvent) => {
        if (this._handlers) {
            this._handlers.onError(new Error("WebSocket error " + this._url));
        }
        if (this.ws) {
            this.ws.close();
        }
    };
    handleClose = (ev: CloseEvent) => {
        if (this._handlers) {
            this.status = WebSocketServiceStatus.Close;
            this._handlers.onClose(ev);
        }
    };
    handleMessage = (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        if (this._handlers) {
            this._handlers.onMessage(data);
        } else {
            this.shutdown();
        }
    };
    send = (message: string) => {
        if (this.status === WebSocketServiceStatus.Open && this.ws) {
            try {
                this.ws.send(message);
            } catch (e) {
                // this.status may have changed between if above and ws.send
                if (this._handlers) {
                    this._handlers.onError(e);
                }
            }
        }
    };
}

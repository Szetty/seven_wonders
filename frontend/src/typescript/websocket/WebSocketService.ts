import * as Uuid from "uuid";
import * as _ from "underscore";

import * as data from "./data";
import * as interfaces from "./interfaces";
import { WebSocketConnection } from "./WebSocketConnection";

interface InFlight {
    ts: number;
    uuid: string;
    data: data.MessageReqToServer;
    resolve: (data: data.MessageAckFromServer) => void;
    reject: (reason?: any) => void;
    retry_policy: interfaces.RetryPolicy;
    timed_out_at_ts: number;
    diagnostics_callback?: (diagnostics: interfaces.DiagnosticsData) => void;
}

export class WebSocketService implements interfaces.WebSocketService {
    private readonly pulseMs = 1000; // default value for pulse
    private readonly reconnectTimeoutMs = 15_000;
    private readonly wsUrl;

    // (Re)connection-specific structures
    private wsConnection: WebSocketConnection | null = null;
    private reconnecting = false;
    private nextReconnectTs = 0;
    private reconnectCounter = 0;
    private isOffline = true;
    private lastIncomingMessageTimestamp: number;
    private completed = false;

    // Session (user)-specific structures. These need to be reset when token changes.
    private inFlightList: InFlight[] = [];

    // Debug
    private debug = {
        send: this.doSend.bind(this),
        onMsg: this.onWsMessage.bind(this),
    };

    // Client

    constructor(wsUrl: string, private _proxy: interfaces.WebSocketProxy) {
        const protocol = process.env["NODE_ENV"] == "production" ? "wss://" : "ws://";
        this.wsUrl = `${protocol}${window.location.host}${wsUrl}`;
        setInterval(this.pulse, this.pulseMs);
    }

    // PUBLIC API

    setProxy(proxy: interfaces.WebSocketProxy) {
        this._proxy = proxy;
    }

    // before running forceReconnect you can check how soon reconnect is going to happen by itself
    // 0 means it's going to happen on next `pulse` tick
    // -1 means that proto is online and no reconnect is scheduled
    timeBeforeNextReconnectMs() {
        if (this.isOffline) {
            let now = new Date().getTime();
            const timeLeft = this.nextReconnectTs - now;
            if (timeLeft > 0) {
                return timeLeft;
            } else {
                return 0;
            }
        } else {
            return -1;
        }
    }

    // forces reconnect to happen on next `pulse` tick
    // it is safe to call this method if proto is already online or was going to reconnect at the same time anyway
    resetReconnectTimer() {
        this.nextReconnectTs = 0;
        this.reconnectCounter = 1;
    }

    // restarts proto connection and resets reconnect timer so that exponential back-off strategy will have small delay in case if server still doesn't respond
    forceReconnect() {
        this.resetReconnectTimer();
        if (!this.isOffline) {
            this.goOffline("forceReconnect");
        }
        this.reconnecting = false;
        this.pulse();
    }

    setDebugOptions(options: interfaces.DebugOptions) {
        const { wsLatency = 100, wsSendDelay = 100, wsReceiveDelay = 100 } = options;

        const throttledRunner = (delay: number, latency: number) => {
            let lastRun = 0;
            return (cb: () => void) => {
                const now = new Date().getTime();
                const current = Math.max(now, lastRun);
                lastRun = current + delay;
                const delayPos = current - now;
                // console.time(`${current / 1000} ${delayPos}`);
                setTimeout(() => {
                    // console.timeEnd(`${current / 1000} ${delayPos}`);
                    cb();
                }, delayPos + latency);
            };
        };
        const send = throttledRunner(wsSendDelay, wsLatency);
        this.doSend = message => {
            send(() => {
                this.debug.send(message);
            });
        };
        const onMsg = throttledRunner(wsReceiveDelay, wsLatency);
        this.onWsMessage = message => {
            onMsg(() => {
                this.debug.onMsg(message);
            });
        };
    }

    public sendMessage(message: data.MessageReqToServer) {
        return this.sendOne<data.MessageFromServer>(message)
    }

    public close() {
        if (this.wsConnection) {
            this.wsConnection.shutdown();
        }
        // this.completed = true;
    }

    // Below we handle responses to our messages and new events

    private static isAckEnvelope(raw: data.EnvelopeFromServer): raw is data.EnvelopeAckFromServer {
        return !!(raw as data.EnvelopeAckFromServer).ack_uuids;
    }

    private onWsMessage(payload: data.BundleFromServer) {
        this.lastIncomingMessageTimestamp = new Date().getTime();
        let ackUuids: string[] = [];

        _.each(payload, raw => {
            if (WebSocketService.isAckEnvelope(raw)) {
                const { data, ack_uuids: ackUuids } = raw;
                // Responses to (some) in-flight messages
                const [confirmations, newInFlightList] = _.partition(this.inFlightList, item => {
                    return ackUuids.indexOf(item.uuid) !== -1;
                });
                this.inFlightList = newInFlightList;
                confirmations.map(item => {
                    const { resolve, reject } = item;
                    if (typeof data === "object" && data.type === "error") {
                        this.collectDiagnostics(item, raw, data as data.ErrorFromServer);
                        reject(data);
                    } else {
                        this.collectDiagnostics(item, raw);
                        resolve(data);
                    }
                });
            } else {
                const { uuid, data } = raw;
                // brand new message, not a response
                this.onIncomingMessage(data);
                if (uuid) {
                    ackUuids.push(uuid);
                } else {
                    this.reportError("incoming_message_uuid_missing", new Error(), raw);
                }
            }
        });

        if (ackUuids.length > 0) {
            this.sendAck(ackUuids);
        }
    }

    // handle new event (not a response)
    private onIncomingMessage(data: data.MessageReqFromServer) {
        if (typeof data !== "object") {
            return;
        }
        if (data.type === "welcome") {
            this.onSync(data as data.WelcomeFromServer);
        } else {
            this._proxy.onIncomingMessage(data as data.MessageFromServer);
        }
    }

    private reportError(type: interfaces.ErrorType, error: Error, info?: any) {
        if (this._proxy.onError) {
            this._proxy.onError({ type, error, info });
        } else {
            console.error(type, error, info);
        }
    }

    // HELPERS

    private static mergeMessageOptions(
        options?: interfaces.MessageOptions,
        defaultRetryPolicy: interfaces.RetryPolicy = "same_session",
        defaultTimeoutMs = 5000
    ) {
        const {
            retry_policy: retryPolicy,
            timeout_milliseconds: timeoutMs,
            tag,
            diagnostics_callback,
            uuid,
        } = typeof options === "object" ? options : ({} as interfaces.MessageOptions);
        return {
            retry_policy: retryPolicy || defaultRetryPolicy,
            timeout_milliseconds: timeoutMs || defaultTimeoutMs,
            tag,
            diagnostics_callback,
            uuid,
        };
    }

    // WEBSOCKET HELPERS

    private sendOne<ReplyType extends data.MessageAckFromServer>(
        data: data.MessageReqToServer,
        options?: interfaces.MessageOptions
    ) {
        const ts = new Date().getTime();
        const {
            retry_policy,
            timeout_milliseconds,
            tag,
            diagnostics_callback,
            uuid: id,
        } = WebSocketService.mergeMessageOptions(options);
        const uuid = id || (typeof tag === "string" ? tag + ":" : "") + Uuid.v1();
        const timed_out_at_ts = ts + timeout_milliseconds;
        const item = {
            ts,
            uuid,
            data,
            retry_policy,
            timed_out_at_ts,
            diagnostics_callback,
        };
        const out = [
            {
                uuid: uuid,
                data: data,
            },
        ];
        this.doSend(out);
        return new Promise((resolve: (replyMessage: ReplyType) => void, reject) => {
            this.inFlightList.push({ ...item, resolve, reject });
        });
    }

    // Drop/reject all in-flight messages that a) timed out or b) don't qualify for resending
    private reconcileInFlight(rejectNonResendableMessages: boolean) {
        const now = new Date().getTime();
        const newInFlightList = [] as InFlight[];
        this.inFlightList.map(item => {
            const { timed_out_at_ts, reject, retry_policy } = item;
            const canBeResent = retry_policy === "any_session";
            if (now >= timed_out_at_ts) {
                const error = { type: "error" as "error", body: { code: "client_timeout" } };
                this.collectDiagnostics(item, undefined, error);
                reject(error);
            } else if (rejectNonResendableMessages && !canBeResent) {
                const error = { type: "error" as "error", body: { code: "session_expired" } };
                this.collectDiagnostics(item, undefined, error);
                reject(error);
            } else {
                newInFlightList.push(item);
            }
        });
        this.inFlightList = newInFlightList;
    }

    private resendInFlightList() {
        this.reconcileInFlight(true);
        if (this.inFlightList.length > 0) {
            let out = _.map(this.inFlightList, ({ uuid, data }) => {
                return {
                    uuid,
                    data,
                };
            });
            this.doSend(out);
        }
    }

    private sendAck(ackUuids: string[]) {
        this.doSend([
            {
                ack_uuids: ackUuids,
                data: "ack",
            },
        ]);
    }

    private isInsideBundle = false;

    public bundle<T>(fn: () => T) {
        const prev = this.isInsideBundle;
        this.isInsideBundle = true;
        let r: T;
        try {
            r = fn();
        } finally {
            this.isInsideBundle = prev;
            // flush only most outer bundle
            if (!prev) {
                this.flushBuffer();
            }
        }
        // there's no catch so r can be undefined inside finally block but not after it
        return r;
    }
    private buffer: data.BundleToServer = [];
    private doSend(message: data.BundleToServer) {
        if (this.isInsideBundle) {
            this.buffer = this.buffer.concat(message);
        } else {
            this.sendToWs(message);
        }
    }
    private flushBuffer = () => {
        if (this.buffer.length) {
            this.sendToWs(this.buffer);
            this.buffer = [];
        }
    };

    private sendToWs = (envelopes: data.EnvelopeToServer[]) => {
        if (this.wsConnection) {
            this.wsConnection.send(JSON.stringify(envelopes));
        }
    };

    private pulse = () => {
        this.reconcileInFlight(false);
        let now = new Date().getTime();

        if (this.isOffline && !this.completed) {
            this.offlinePulse();
        }
    };

    private goOnline() {
        this.resetReconnectIfConnectionIsGood.onOnline();
        this.isOffline = false;
        this.reconnecting = false;
        this._proxy.onOnline();
    }

    private goOffline(reason: string) {
        this.resetReconnectIfConnectionIsGood.onOffline();
        this.reportError("going_offline", new Error(), reason);
        this.wsConnection && this.wsConnection.shutdown();
        this.isOffline = true;
        this._proxy.onOffline();
    }

    private resetReconnectIfConnectionIsGood = {
        // it's possible to get disconnected right after hello, so we can't reset reconnect
        // timer right after hello, we need to wait a bit
        timer: undefined as ReturnType<typeof setTimeout> | undefined,
        onOnline: () => {
            this.resetReconnectIfConnectionIsGood.timer = setTimeout(() => {
                this.reconnectCounter = 0;
            }, this.reconnectTimeoutMs);
            // it doesn't really matter what timeout we use here
        },
        onOffline: () => {
            if (this.resetReconnectIfConnectionIsGood.timer) {
                clearTimeout(this.resetReconnectIfConnectionIsGood.timer);
            }
        },
    };

    private onWsClose(ev: CloseEvent) {
        this.reconnecting = false;
        if (!this.isOffline) {
            this.goOffline(`ws closed ${ev.code} ${ev.reason}`);
        }
        if (ev.code === 1000 && ev.reason) {
            // fatal error on server
            let reason = ev.reason as any;
            try {
                reason = JSON.parse(reason);
            } catch (e) {}
            this.reportError("websocket_fatal_error", new Error(), reason);
            this.inFlightList.map(item => {
                const { reject } = item;
                const error = {
                    type: "fatal_error",
                    reason: reason,
                } as const;
                this.collectDiagnostics(item, undefined, error);
                reject(error);
            });
            this.inFlightList = [];
        }
    }

    private onWsConnected() {
        this.onConnected();
    }

    private onWsError(error: Error) {
        this.reportError("websocket_error", error);
    }

    private onConnected() {
        this.goOnline();
    }

    private onSync(data: data.WelcomeFromServer) {
        this.resendInFlightList();
        this._proxy.onSync(data);
    }

    private offlinePulse() {
        // Make sure we're trying to reconnect
        if (!this.reconnecting && this.reconnectDelay()) {
            this.reconnecting = true;
            if (this.wsConnection) {
                this.wsConnection.shutdown();
            }
            this.wsConnection = this.createWsConnection(this.wsUrl);
        } else if (this.reconnecting) {
            // let's see if reconnect takes too long
            if (
                this.wsConnection &&
                this.wsConnection.uptime > this.reconnectTimeoutMs
            ) {
                this.reportError("reconnecting_error", new Error(), this.wsConnection);
                this.reconnecting = false;
            }
        }
    }

    // we don't want to reconnect too often, that's why we can skip some
    // pulses before reconnecting
    private reconnectDelay() {
        let now = new Date().getTime();
        if (this.timeBeforeNextReconnectMs() <= 0) {
            const maxCount = Math.min(this.reconnectCounter, 7);
            const timeoutSec = Math.pow(2, maxCount + Math.random());
            this.nextReconnectTs = now + timeoutSec * 1000;
            this.reconnectCounter++;
            return true;
        }
        return false;
    }

    private createWsConnection(url: string) {
        return new WebSocketConnection(WebSocket, url, {
            onMessage: data => this.onWsMessage(data),
            onConnected: () => this.onWsConnected(),
            onClose: ev => this.onWsClose(ev),
            onError: error => this.onWsError(error),
        });
    }

    private collectDiagnostics(
        item: Omit<InFlight, "resolve" | "reject">,
        raw?: data.EnvelopeAckFromServer,
        reject_reason?: data.ErrorFromServer | { type: "fatal_error"; reason: any }
    ) {
        const { diagnostics_callback } = item;
        const { onDiagnostics } = this._proxy;
        if (diagnostics_callback || onDiagnostics) {
            const now = new Date().getTime();
            const { ts, uuid, data, retry_policy, timed_out_at_ts } = item;
            const info = {
                ack_elapsed: raw && raw.ack_elapsed && raw.ack_elapsed[uuid],
                now,
                ts,
                uuid,
                data,
                reject_reason,
                retry_policy,
                timed_out_at_ts,
            };
            if (diagnostics_callback) {
                diagnostics_callback(info);
            }
            if (onDiagnostics) {
                onDiagnostics(info);
            }
        }
    }
}

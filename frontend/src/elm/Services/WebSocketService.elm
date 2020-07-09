module Services.WebSocketService exposing (..)

import Json.Decode exposing (Decoder, decodeString, field, string)
import Json.Encode exposing (Value, encode)
import Networking.WebSocket as WebSocket


type WSEventType body
    = Incoming body
    | Offline
    | Online
    | Sync
    | Reply body
    | Error String


init : String -> String -> Cmd msg
init userToken url =
    WebSocket.initWebSocket (url ++ "?authorization=" ++ userToken)


sendMessage : Value -> Cmd msg
sendMessage message =
    WebSocket.sendWSMessage (encode 0 message)


close : Cmd msg
close =
    WebSocket.closeWS ""


subscriptions : (String -> Decoder body) -> (WSEventType body -> msg) -> Sub msg
subscriptions messageDecoder toMsg =
    let
        eventWithoutPayload eventType _ =
            toMsg eventType

        event eventType payload =
            case decodeString (field "type" string) payload of
                Ok messageType ->
                    case decodeString (field "body" <| messageDecoder messageType) payload of
                        Ok a ->
                            toMsg (eventType a)

                        Err _ ->
                            toMsg (Error "decoding body failed")

                Err _ ->
                    toMsg (Error "decoding type failed")
    in
    Sub.batch
        [ WebSocket.incomingWSMessage (event Incoming)
        , WebSocket.onWSOffline (eventWithoutPayload Offline)
        , WebSocket.onWSOnline (eventWithoutPayload Online)
        , WebSocket.onWSSync (eventWithoutPayload Sync)
        , WebSocket.replyWSMessage (event Reply)
        ]

module Services.WebSocketService exposing (..)

import Common.Session exposing (Session(..))
import Networking.WebSocket as WebSocket


init : Session -> String -> Cmd msg
init session url =
    case session of
        Guest _ ->
            Cmd.none

        LoggedIn _ userInfo ->
            WebSocket.initWebSocket (url ++ "/" ++ userInfo.gameID ++ "?authorization=" ++ userInfo.userToken)


sendMessage : String -> Cmd msg
sendMessage message =
    WebSocket.sendWSMessage message


close : Cmd msg
close =
    WebSocket.closeWS ""


subscriptions : (String -> msg) -> Sub msg
subscriptions toMsg =
    let
        event name payload =
            toMsg (name ++ payload)
    in
    Sub.batch
        [ WebSocket.incomingWSMessage toMsg
        , WebSocket.onWSOffline (event "Offline")
        , WebSocket.onWSOnline (event "Online")
        , WebSocket.onWSSync (event "Sync")
        , WebSocket.replyWSMessage toMsg
        ]

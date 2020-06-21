module Services.GameService exposing (..)

import Common.Session exposing (Session(..))
import Networking.Endpoints as Endpoints
import Networking.WebSocket as WebSocket


startWebSocket : Session -> Cmd msg
startWebSocket session =
    case session of
        Guest _ ->
            Cmd.none

        LoggedIn _ userInfo ->
            WebSocket.initWebSocket (Endpoints.game ++ "/" ++ userInfo.gameID ++ "?authorization=" ++ userInfo.userToken)


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
        , WebSocket.onWSError (event "Error")
        , WebSocket.replyWSMessage toMsg
        ]

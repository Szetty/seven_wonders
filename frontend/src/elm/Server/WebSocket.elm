port module Server.WebSocket exposing (..)

import Common.Session exposing (Session(..))
import Server.Endpoints as Endpoints


startWebSocket : Session -> Cmd msg
startWebSocket session =
    case session of
        Guest _ ->
            Cmd.none

        LoggedIn _ userInfo ->
            initWebSocket (Endpoints.game ++ "/" ++ userInfo.gameID ++ "?authorization=" ++ userInfo.userToken)


port initWebSocket : String -> Cmd msg


port sendWSMessage : String -> Cmd msg


port onWSSync : (String -> msg) -> Sub msg


port onWSOffline : (String -> msg) -> Sub msg


port onWSOnline : (String -> msg) -> Sub msg


port incomingWSMessage : (String -> msg) -> Sub msg


port replyWSMessage : (String -> msg) -> Sub msg

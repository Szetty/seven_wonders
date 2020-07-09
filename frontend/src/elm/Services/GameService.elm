module Services.GameService exposing (..)

import Common.Session exposing (Session(..))
import Json.Decode exposing (string)
import Networking.Endpoints as Endpoints
import Services.WebSocketService as WebSocketService


type Message
    = Offline
    | Online
    | Sync
    | Error String
    | Incoming String
    | Reply String


initGame : Session -> Cmd msg
initGame session =
    case session of
        Guest _ ->
            Cmd.none

        LoggedIn _ userInfo ->
            let
                url =
                    String.replace ":gameID" userInfo.gameID Endpoints.game
            in
            WebSocketService.init userInfo.userToken url


subscriptions : (Message -> msg) -> Sub msg
subscriptions toMsg =
    let
        mapper wsEventType =
            toMsg (wsEventToMessage wsEventType)
    in
    WebSocketService.subscriptions (\_ -> string) mapper


wsEventToMessage : WebSocketService.WSEventType String -> Message
wsEventToMessage wsEventType =
    case wsEventType of
        WebSocketService.Offline ->
            Offline

        WebSocketService.Online ->
            Online

        WebSocketService.Sync ->
            Sync

        WebSocketService.Error error ->
            Error error

        WebSocketService.Incoming body ->
            Incoming body

        WebSocketService.Reply reply ->
            Reply reply

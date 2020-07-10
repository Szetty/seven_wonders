module Services.LobbyService exposing (..)

import Common.Session exposing (Session(..))
import Json.Decode as Decode exposing (Decoder, bool, field, list, string, succeed)
import Json.Encode as Encode
import Networking.Endpoints as Endpoints
import Services.WebSocketService as WebSocketService


type Message
    = Offline
    | Online
    | Sync
    | Error String
    | Incoming MessageBody
    | Reply MessageBody


type MessageBody
    = OnlineUsersReply (List String)
    | InvitedUsersReply (List InvitedUser)
    | InviteUserReply String
    | UninviteUserReply String
    | GotInvite String
    | GotUninvite String
    | UserGotOnline String
    | UserGotOffline String
    | Unknown String


type alias InvitedUser =
    { name : String
    , connected : Bool
    }


initGameLobby : Session -> Cmd msg
initGameLobby session =
    case session of
        Guest _ ->
            Cmd.none

        LoggedIn _ userInfo ->
            let
                url =
                    String.replace ":gameID" userInfo.gameID Endpoints.gameLobby
            in
            WebSocketService.init userInfo.userToken url


getOnlineUsers : Cmd msg
getOnlineUsers =
    let
        payload =
            Encode.object
                [ ( "type", Encode.string "OnlineUsers" ) ]
    in
    WebSocketService.sendMessage payload


getInvitedUsers : Cmd msg
getInvitedUsers =
    let
        payload =
            Encode.object
                [ ( "type", Encode.string "InvitedUsers" ) ]
    in
    WebSocketService.sendMessage payload


inviteUser : String -> Cmd msg
inviteUser toInvite =
    let
        payload =
            Encode.object
                [ ( "type", Encode.string "InviteUser" )
                , ( "body", Encode.string toInvite )
                ]
    in
    WebSocketService.sendMessage payload


uninviteUser : String -> Cmd msg
uninviteUser toUninvite =
    let
        payload =
            Encode.object
                [ ( "type", Encode.string "UninviteUser" )
                , ( "body", Encode.string toUninvite )
                ]
    in
    WebSocketService.sendMessage payload


subscriptions : (Message -> msg) -> Sub msg
subscriptions toMsg =
    let
        mapper wsEventType =
            toMsg (wsEventToMessage wsEventType)
    in
    WebSocketService.subscriptions messageBodyDecoder mapper


wsEventToMessage : WebSocketService.WSEventType MessageBody -> Message
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


messageBodyDecoder : String -> Decoder MessageBody
messageBodyDecoder messageType =
    case messageType of
        "OnlineUsersReply" ->
            Decode.map OnlineUsersReply (list string)

        "InvitedUsersReply" ->
            let
                invitedUserDecoder =
                    Decode.map2 InvitedUser (field "name" string) (field "connected" bool)
            in
            Decode.map InvitedUsersReply (list invitedUserDecoder)

        "InviteUserReply" ->
            Decode.map InviteUserReply string

        "UninviteUserReply" ->
            Decode.map UninviteUserReply string

        "GotInvite" ->
            Decode.map GotInvite string

        "GotUninvite" ->
            Decode.map GotUninvite string

        "UserGotOnline" ->
            Decode.map UserGotOnline string

        "UserGotOffline" ->
            Decode.map UserGotOffline string

        mType ->
            Decode.map Unknown (succeed mType)

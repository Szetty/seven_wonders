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
    | UninviteUserReply
    | GotInvite User
    | GotUninvite String
    | UserGotOnline String
    | UserGotOffline String
    | Connected String
    | DeclinedInvitation String
    | DeclineInvitationReply
    | Unknown String


type alias InvitedUser =
    { name : String
    , connected : Bool
    , leader : Bool
    }


type alias User =
    { name : String
    , gameID : String
    }


initGameLobby : Session -> Cmd msg
initGameLobby session =
    case session of
        Guest _ ->
            Cmd.none

        LoggedIn _ sessionData ->
            let
                url =
                    String.replace ":gameID" sessionData.userInfo.gameID Endpoints.gameLobby
            in
            WebSocketService.init sessionData.userInfo.userToken url


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


declineInvitation : String -> Cmd msg
declineInvitation gameID =
    let
        payload =
            Encode.object
                [ ( "type", Encode.string "DeclineInvitation" )
                , ( "body", Encode.string gameID )
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
                    Decode.map3 InvitedUser (field "name" string) (field "connected" bool) (field "leader" bool)
            in
            Decode.map InvitedUsersReply (list invitedUserDecoder)

        "InviteUserReply" ->
            Decode.map InviteUserReply string

        "UninviteUserReply" ->
            Decode.succeed UninviteUserReply

        "DeclineInvitationReply" ->
            Decode.succeed DeclineInvitationReply

        "GotInvite" ->
            let
                user =
                    Decode.map2 User (field "name" string) (field "gameID" string)
            in
            Decode.map GotInvite user

        "GotUninvite" ->
            Decode.map GotUninvite string

        "UserGotOnline" ->
            Decode.map UserGotOnline string

        "UserGotOffline" ->
            Decode.map UserGotOffline string

        "Connected" ->
            Decode.map Connected string

        "DeclinedInvitation" ->
            Decode.map DeclinedInvitation string

        mType ->
            Decode.map Unknown (succeed mType)


closeLobby : Cmd msg
closeLobby =
    WebSocketService.close

module Services.WebStorageService exposing (..)

import Common.Domain as Domain exposing (SavedNotification, SessionData, UserInfo)
import Json.Encode as Encode
import Networking.WebStorage as WebStorage


saveUserInfo : UserInfo -> Cmd msg
saveUserInfo userInfo =
    let
        encoded =
            Encode.encode 0 <| Domain.userInfoEncoder userInfo
    in
    WebStorage.storeUserInfo encoded


saveNotifications : Maybe (List SavedNotification) -> Cmd msg
saveNotifications savedNotifications =
    case savedNotifications of
        Just notifications ->
            let
                encoded =
                    Encode.encode 0 <| Domain.notificationsEncoder notifications
            in
            WebStorage.storeNotifications encoded

        Nothing ->
            Cmd.none


deleteSessionData : Cmd msg
deleteSessionData =
    WebStorage.clearStorage ""

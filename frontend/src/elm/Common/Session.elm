module Common.Session exposing (..)

import Browser.Navigation as Nav
import Common.Domain exposing (Notification, SavedNotification, SessionData, UserInfo)


type Session
    = LoggedIn Nav.Key SessionData
    | Guest Nav.Key


initSession : Nav.Key -> Maybe SessionData -> Session
initSession navKey sessionDataMaybe =
    case sessionDataMaybe of
        Just sessionData ->
            LoggedIn navKey sessionData

        Nothing ->
            Guest navKey


getNavKey : Session -> Nav.Key
getNavKey session =
    case session of
        LoggedIn key _ ->
            key

        Guest key ->
            key


getUserInfo : Session -> Maybe UserInfo
getUserInfo session =
    case session of
        LoggedIn _ sessionData ->
            Just sessionData.userInfo

        Guest _ ->
            Nothing


getSavedNotifications : Session -> Maybe (List SavedNotification)
getSavedNotifications session =
    case session of
        LoggedIn _ sessionData ->
            Just sessionData.notifications

        Guest _ ->
            Nothing


getCurrentUsername : Session -> String
getCurrentUsername session =
    case Maybe.map .name (getUserInfo session) of
        Just n ->
            n

        Nothing ->
            "Anonymous"


getGameId : Session -> Maybe String
getGameId session =
    Maybe.map .gameID (getUserInfo session)


toLoggedIn : Session -> UserInfo -> Session
toLoggedIn session userInfo =
    let
        sessionData =
            SessionData userInfo []
    in
    LoggedIn (getNavKey session) sessionData

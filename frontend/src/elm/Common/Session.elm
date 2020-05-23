module Common.Session exposing (..)

import Browser.Navigation as Nav


type Session
    = LoggedIn Nav.Key UserInfo
    | Guest Nav.Key


type alias UserInfo =
    { name : String
    , userToken : String
    , gameID : String
    }


initSession : Nav.Key -> Maybe UserInfo -> Session
initSession navKey userInfoMaybe =
    case userInfoMaybe of
        Just userInfo ->
            LoggedIn navKey userInfo

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
        LoggedIn _ userToken ->
            Just userToken

        Guest _ ->
            Nothing


setUserToken session userToken =
    LoggedIn (getNavKey session) userToken

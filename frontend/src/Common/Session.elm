module Common.Session exposing (..)

import Browser.Navigation as Nav


type Session
    = LoggedIn Nav.Key String
    | Guest Nav.Key


getNavKey : Session -> Nav.Key
getNavKey session =
    case session of
        LoggedIn key _ ->
            key

        Guest key ->
            key

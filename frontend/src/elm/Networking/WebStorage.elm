port module Networking.WebStorage exposing (..)


port storeUserInfo : String -> Cmd msg


port storeNotifications : String -> Cmd msg


port clearStorage : String -> Cmd msg

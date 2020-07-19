port module Networking.WebStorage exposing (..)


port storeUserInfo : String -> Cmd msg


port deleteUserInfo : String -> Cmd msg

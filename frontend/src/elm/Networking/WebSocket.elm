port module Networking.WebSocket exposing (..)


port initWebSocket : String -> Cmd msg


port sendWSMessage : String -> Cmd msg


port closeWebSocket : String -> Cmd msg


port onWSSync : (String -> msg) -> Sub msg


port onWSOffline : (String -> msg) -> Sub msg


port onWSOnline : (String -> msg) -> Sub msg


port onWSError : (String -> msg) -> Sub msg


port incomingWSMessage : (String -> msg) -> Sub msg


port replyWSMessage : (String -> msg) -> Sub msg

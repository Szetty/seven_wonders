module Networking.Http exposing (..)

import Http


post : String -> Http.Body -> Http.Expect msg -> Cmd msg
post url body expect =
    postWithHeaders url body expect []


postWithHeaders : String -> Http.Body -> Http.Expect msg -> List Http.Header -> Cmd msg
postWithHeaders url body expect headers =
    Http.request
        { method = "POST"
        , headers = headers
        , url = url
        , body = body
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }


get : String -> Http.Body -> Http.Expect msg -> Cmd msg
get url body expect =
    getWithHeaders url body expect []


getWithHeaders : String -> Http.Body -> Http.Expect msg -> List Http.Header -> Cmd msg
getWithHeaders url body expect headers =
    Http.request
        { method = "GET"
        , headers = headers
        , url = url
        , body = body
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }

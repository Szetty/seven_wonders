module Server.LogoutService exposing (..)

import Common.Session exposing (UserInfo)
import Http exposing (emptyBody, header)
import Server.Common as Common exposing (ErrorBody, FailedRequest)
import Server.Endpoints as Endpoints


type Msg
    = GotLogoutResponse (Result FailedRequest ())


logout : UserInfo -> Cmd Msg
logout userInfo =
    Http.request
        { method = "POST"
        , headers = [ header "Authorization" ("Bearer " ++ userInfo.userToken) ]
        , url = Endpoints.logout
        , body = emptyBody
        , expect = Common.expectWhatever GotLogoutResponse
        , timeout = Nothing
        , tracker = Nothing
        }


tryExtractResponse : Msg -> ( Result ErrorBody (), Cmd Msg )
tryExtractResponse (GotLogoutResponse response) =
    Common.tryExtractResponse response

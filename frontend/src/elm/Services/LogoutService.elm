module Services.LogoutService exposing (..)

import Common.Domain exposing (UserInfo)
import Common.Error exposing (ErrorBody)
import Http exposing (emptyBody, header)
import Networking.Endpoints as Endpoints
import Networking.Http exposing (postWithHeaders)
import Networking.HttpResponse as HttpResponse


type Msg
    = GotLogoutResponse (Result HttpResponse.FailedRequest ())


logout : UserInfo -> Cmd Msg
logout userInfo =
    let
        headers =
            [ header "Authorization" ("Bearer " ++ userInfo.userToken) ]

        expect =
            HttpResponse.expectWhatever GotLogoutResponse
    in
    postWithHeaders Endpoints.logout emptyBody expect headers


tryExtractResponse : Msg -> ( Result ErrorBody (), Cmd Msg )
tryExtractResponse (GotLogoutResponse response) =
    HttpResponse.tryExtractResponse response

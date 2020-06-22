module Services.TokenService exposing (..)

import Common.Error exposing (ErrorBody)
import Http exposing (emptyBody, header)
import Networking.Endpoints as Endpoints
import Networking.Http exposing (postWithHeaders)
import Networking.HttpResponse as HttpResponse


type Msg
    = GotResponse (Result HttpResponse.FailedRequest ())


type alias Body =
    { token : String
    }


checkToken : Body -> Cmd Msg
checkToken body =
    let
        headers =
            [ header "Authorization" ("Bearer " ++ body.token) ]

        expect =
            HttpResponse.expectWhatever GotResponse
    in
    postWithHeaders Endpoints.checkToken emptyBody expect headers


tryExtractResponse : Msg -> ( Result ErrorBody (), Cmd Msg )
tryExtractResponse (GotResponse response) =
    HttpResponse.tryExtractResponse response

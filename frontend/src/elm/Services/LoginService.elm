module Services.LoginService exposing (..)

import Http as Http
import Json.Decode as Decoder
import Json.Encode as Encoder
import Networking.Endpoints as Endpoints
import Networking.Http exposing (post)
import Networking.HttpResponse as HttpResponse


type Msg
    = GotResponse (Result HttpResponse.FailedRequest Response)


type alias Body =
    { name : String
    , accessToken : String
    }


type alias Response =
    { userToken : String
    , name : String
    , gameID : String
    }


login : Body -> Cmd Msg
login body =
    let
        httpBody =
            Http.jsonBody <|
                Encoder.object
                    [ ( "name", Encoder.string body.name ), ( "access_token", Encoder.string body.accessToken ) ]

        loginResponseDecoder =
            Decoder.map3 Response
                (Decoder.field "user_token" Decoder.string)
                (Decoder.field "name" Decoder.string)
                (Decoder.field "game_id" Decoder.string)

        expect =
            HttpResponse.expectJson GotResponse loginResponseDecoder
    in
    post Endpoints.login httpBody expect


tryExtractResponse : Msg -> ( Result HttpResponse.ErrorBody Response, Cmd Msg )
tryExtractResponse (GotResponse response) =
    HttpResponse.tryExtractResponse response

module Server.Login exposing (..)

import Http exposing (jsonBody)
import Json.Decode as Decoder
import Json.Encode as Encoder
import Server.Common as Common
import Server.Endpoints as Endpoints


type Msg
    = GotResponse (Result Http.Error Response)


type alias Body =
    { name : String
    , accessToken : String
    }


type alias Response =
    { user_token : String }


login : Body -> Cmd Msg
login body =
    let
        encodedBody =
            Encoder.object
                [ ( "name", Encoder.string body.name ), ( "access_token", Encoder.string body.accessToken ) ]

        loginResponseDecoder =
            Decoder.map Response
                (Decoder.field "user_token" Decoder.string)
    in
    Http.post
        { url = Endpoints.login
        , body = jsonBody encodedBody
        , expect = Http.expectJson GotResponse loginResponseDecoder
        }


extractResponse : Msg -> Maybe Response
extractResponse (GotResponse response) =
    Common.extractResponse response

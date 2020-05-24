module Server.LoginService exposing (..)

import Http exposing (jsonBody)
import Json.Decode as Decoder
import Json.Encode as Encoder
import Server.Common as Common exposing (ErrorBody, FailedRequest)
import Server.Endpoints as Endpoints


type Msg
    = GotResponse (Result FailedRequest Response)


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
        encodedBody =
            Encoder.object
                [ ( "name", Encoder.string body.name ), ( "access_token", Encoder.string body.accessToken ) ]

        loginResponseDecoder =
            Decoder.map3 Response
                (Decoder.field "user_token" Decoder.string)
                (Decoder.field "name" Decoder.string)
                (Decoder.field "game_id" Decoder.string)
    in
    Http.post
        { url = Endpoints.login
        , body = jsonBody encodedBody
        , expect = Common.expectJson GotResponse loginResponseDecoder
        }


extractResponse : Msg -> Result ErrorBody Response
extractResponse (GotResponse response) =
    Common.extractResponse response

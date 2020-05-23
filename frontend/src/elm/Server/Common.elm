module Server.Common exposing (..)

import Http exposing (Error(..), Expect, expectStringResponse)
import Json.Decode as D


type FailedRequest
    = BadUrl String
    | Timeout
    | NetworkError
    | BadRequest Http.Metadata ErrorBody
    | ServerError Http.Metadata String
    | BadBody Http.Metadata String String


type alias ErrorBody =
    { errorType : ErrorType
    , errorMessage : String
    }


type ErrorType
    = InvalidName
    | InvalidBody
    | CannotParsePayload
    | InvalidAccessToken
    | Unauthorized
    | InvalidEndpoint


extractResponse : Result FailedRequest a -> Maybe a
extractResponse result =
    case result of
        Ok response ->
            Just response

        Err error ->
            let
                _ =
                    Debug.log "HTTP request failed" error
            in
            Nothing


expectJson : (Result FailedRequest a -> msg) -> D.Decoder a -> Expect msg
expectJson toMsg decoder =
    expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (BadUrl url)

                Http.Timeout_ ->
                    Err Timeout

                Http.NetworkError_ ->
                    Err NetworkError

                Http.BadStatus_ metadata body ->
                    if metadata.statusCode >= 400 && metadata.statusCode < 500 then
                        case D.decodeString errorDecoder body of
                            Ok errorBody ->
                                Err (BadRequest metadata errorBody)

                            Err err ->
                                Err (BadBody metadata (D.errorToString err) body)

                    else
                        Err (ServerError metadata body)

                Http.GoodStatus_ metadata body ->
                    case D.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (BadBody metadata (D.errorToString err) body)


errorDecoder =
    D.map2 ErrorBody
        (D.field "error_type" errorTypeDecoder)
        (D.field "error_message" D.string)


errorTypeDecoder : D.Decoder ErrorType
errorTypeDecoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "INVALID_NAME" ->
                        D.succeed InvalidName

                    "INVALID_BODY" ->
                        D.succeed InvalidBody

                    "CANNOT_PARSE_PAYLOAD" ->
                        D.succeed CannotParsePayload

                    "INVALID_ACCESS_TOKEN" ->
                        D.succeed InvalidAccessToken

                    "UNAUTHORIZED" ->
                        D.succeed Unauthorized

                    "INVALID_ENDPOINT" ->
                        D.succeed InvalidEndpoint

                    somethingElse ->
                        D.fail <| "Unknown error type: " ++ somethingElse
            )

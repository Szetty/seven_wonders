module Server.Common exposing (..)

import Common.Logger as Logger
import Http exposing (Error(..), Expect, expectStringResponse)
import Json.Decode as D
import Json.Encode as E


type FailedRequest
    = BadUrl String
    | Timeout
    | NetworkError
    | BadRequest Http.Metadata ErrorBody
    | ServerError Http.Metadata String
    | BadBody Http.Metadata String String


type alias ErrorBody =
    { kind : ErrorType
    , message : String
    }


type ErrorType
    = InvalidName
    | InvalidAccessToken
    | InvalidGameId
    | Unauthorized
    | Unknown


{-| Tries to extract response, if successful it returns the response
if not successful it logs the error and returns client errors
-}
tryExtractResponse : Result FailedRequest a -> ( Result ErrorBody a, Cmd msg )
tryExtractResponse result =
    let
        defaultErrorBody =
            ErrorBody Unknown "Something went wrong"
    in
    case result of
        Ok response ->
            ( Ok response, Cmd.none )

        Err error ->
            case error of
                BadUrl msg ->
                    ( Err defaultErrorBody, Logger.log "Bad url" msg )

                Timeout ->
                    ( Err defaultErrorBody, Logger.log "Request Timeout" "" )

                NetworkError ->
                    ( Err defaultErrorBody, Logger.log "Network Error" "" )

                BadRequest metadata errorBody ->
                    case errorBody.kind of
                        InvalidName ->
                            ( Err errorBody, Logger.log ("Invalid name: " ++ errorBody.message) (encodeMetadata metadata) )

                        InvalidAccessToken ->
                            ( Err errorBody, Logger.log ("Invalid access token: " ++ errorBody.message) (encodeMetadata metadata) )

                        InvalidGameId ->
                            ( Err errorBody, Logger.log ("Invalid game id: " ++ errorBody.message) (encodeMetadata metadata) )

                        Unauthorized ->
                            ( Err errorBody, Logger.log ("Unauthorized: " ++ errorBody.message) (encodeMetadata metadata) )

                        Unknown ->
                            ( Err defaultErrorBody, Logger.log ("Unknown bad request: " ++ errorBody.message) (encodeMetadata metadata) )

                ServerError metadata msg ->
                    ( Err defaultErrorBody, Logger.log ("Server Error: " ++ msg) (encodeMetadata metadata) )

                BadBody metadata msg bodyToParse ->
                    ( Err defaultErrorBody, Logger.log ("Could not parse response body: " ++ msg ++ bodyToParse) (encodeMetadata metadata) )


expectWhatever : (Result FailedRequest () -> msg) -> Expect msg
expectWhatever toMsg =
    let
        b _ _ =
            Ok ()
    in
    Http.expectStringResponse toMsg (transformHttpResponseToResult b)


expectJson : (Result FailedRequest a -> msg) -> D.Decoder a -> Expect msg
expectJson toMsg decoder =
    let
        payloadDecoder body metadata =
            case D.decodeString decoder body of
                Ok value ->
                    Ok value

                Err err ->
                    Err (BadBody metadata (D.errorToString err) body)
    in
    expectStringResponse toMsg (transformHttpResponseToResult payloadDecoder)


transformHttpResponseToResult : (String -> Http.Metadata -> Result FailedRequest a) -> Http.Response String -> Result FailedRequest a
transformHttpResponseToResult payloadDecoder response =
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
            payloadDecoder body metadata


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

                    "INVALID_ACCESS_TOKEN" ->
                        D.succeed InvalidAccessToken

                    "UNAUTHORIZED" ->
                        D.succeed Unauthorized

                    "INVALID_GAME_ID" ->
                        D.succeed InvalidGameId

                    _ ->
                        D.succeed Unknown
            )


encodeMetadata : Http.Metadata -> String
encodeMetadata metadata =
    let
        encodedMetadata =
            E.object
                [ ( "url", E.string metadata.url )
                , ( "statusCode", E.int metadata.statusCode )
                , ( "statusText", E.string metadata.statusText )
                , ( "headers", E.dict identity E.string metadata.headers )
                ]
    in
    E.encode 0 encodedMetadata

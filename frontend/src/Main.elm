module Main exposing (main)

import Browser exposing (UrlRequest, application)
import Browser.Navigation exposing (Key)
import Html exposing (button, div, text)
import Html.Events exposing (onClick)
import Http
import Url exposing (Url)


type Msg
    = Init
    | Ping
    | GotPong (Result Http.Error String)


type alias Model =
    String


main : Program () Model Msg
main =
    application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = onUrlChange
        , onUrlRequest = onUrlRequest
        }


init : flags -> Url -> Key -> ( Model, Cmd Msg )
init _ _ _ =
    initialValue


initialValue : ( Model, Cmd Msg )
initialValue =
    ( "", Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Ping ->
            ( model, ping )

        Init ->
            initialValue

        GotPong result ->
            case result of
                Ok response ->
                    ( response, Cmd.none )

                Err Http.Timeout ->
                    ( "TIMEOUT", Cmd.none )

                Err Http.NetworkError ->
                    ( "NETWORK ERROR", Cmd.none )

                Err _ ->
                    ( "FAIL", Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "7 Wonders"
    , body =
        [ div []
            [ button [ onClick Ping ] [ text "PING" ]
            , div [] [ text model ]
            ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


onUrlChange : Url -> Msg
onUrlChange _ =
    Init


onUrlRequest : UrlRequest -> Msg
onUrlRequest _ =
    Init


ping : Cmd Msg
ping =
    Http.get
        { url = "/ping"
        , expect = Http.expectString GotPong
        }

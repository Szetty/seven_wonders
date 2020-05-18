module Main exposing (main)

import Browser exposing (UrlRequest, application)
import Browser.Navigation exposing (Key)
import Html exposing (button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Url exposing (Url)


type Msg
    = Ping
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
        , onUrlChange = NavigationMsg << UrlChanged
        , onUrlRequest = NavigationMsg << LinkClicked
        }


init : flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    initialValue url key


initialValue : ( Model, Cmd Msg )
initialValue =
    ( "", Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Ping ->
            ( model, ping )

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
        [ div [class "mt-3"]
            [ button [ onClick Ping, class "btn btn-primary" ] [ text "PING" ]
            , div [] [ text model ]
            ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


ping : Cmd Msg
ping =
    Http.get
        { url = "/api/ping"
        , expect = Http.expectString GotPong
        }

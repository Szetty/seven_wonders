module Main exposing (main)

import Browser exposing (UrlRequest, application)
import Browser.Navigation as Nav
import Html exposing (button, div, img, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Http
import Image exposing (Image)
import Navigation exposing (Msg(..), Navigation)
import Url exposing (Url)


type Msg
    = Ping
    | GotPong (Result Http.Error String)
    | GotImage (Result Http.Error (Maybe Image))
    | NavigationMsg Navigation.Msg


type alias Model =
    { text : String
    , image : Maybe Image
    , navigation : Navigation
    }


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


initialValue : Url -> Nav.Key -> ( Model, Cmd Msg )
initialValue url key =
    let
        navigation =
            Navigation.init key url
    in
    ( Model "" Nothing navigation, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Ping ->
            ( model, ping )

        GotPong result ->
            case result of
                Ok response ->
                    ( { model | text = response }, Cmd.none )

                Err Http.Timeout ->
                    ( { model | text = "TIMEOUT" }, Cmd.none )

                Err Http.NetworkError ->
                    ( { model | text = "NETWORK ERROR" }, Cmd.none )

                Err _ ->
                    ( { model | text = "FAIL PONG" }, Cmd.none )

        GotImage result ->
            case result of
                Ok response ->
                    ( { model | image = response }, Cmd.none )

                Err _ ->
                    ( { model | text = "FAIL" }, Cmd.none )

        NavigationMsg navigationMsg ->
            let
                ( navigation, navigationCmd ) =
                    Navigation.update navigationMsg model.navigation
            in
            ( { model | navigation = navigation }, Cmd.map NavigationMsg navigationCmd )


view : Model -> Browser.Document Msg
view model =
    { title = "7 Wonders"
    , body =
        [ div [ class "mt-3" ]
            [ button [ onClick Ping, class "btn btn-primary" ] [ text "PING" ]
            , div [] [ text model.text ]
            , div [] []
            , img [ src "%PUBLIC_URL%/wonders/alexandriaA.png" ] []
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

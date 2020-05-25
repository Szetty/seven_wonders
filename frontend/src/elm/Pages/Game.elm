module Pages.Game exposing (..)

import Common.Logger as Logger
import Common.Session exposing (Session)
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Http
import Image exposing (Image)
import Server.WebSocket as WebSocket


type Msg
    = Ping
    | GotPong (Result Http.Error String)
    | GotImage (Result Http.Error (Maybe Image))
    | InitWebSocket
    | GotWS String


type alias Model =
    { session : Session
    , text : String
    , image : Maybe Image
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , text = ""
      , image = Nothing
      }
    , WebSocket.startWebSocket session
    )


toSession : Model -> Session
toSession { session } =
    session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitWebSocket ->
            if model.text == "INIT_WS" then
                let
                    cmd =
                        Cmd.batch
                            [ WebSocket.initWebSocket ""
                            , WebSocket.sendWSMessage ""
                            ]
                in
                ( model, cmd )

            else
                ( model, Cmd.none )

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

        GotWS r ->
            ( model, Logger.log "WS" r )


view : Model -> List (Html Msg)
view model =
    [ div [ class "mt-3" ]
        [ button [ onClick Ping, class "btn btn-primary" ] [ text "PING" ]
        , div [] [ text model.text ]
        , div [] []
        , img [ src "%PUBLIC_URL%/wonders/alexandriaA.png" ] []
        ]
    ]


ping : Cmd Msg
ping =
    Http.get
        { url = "/api/ping"
        , expect = Http.expectString GotPong
        }


subscriptions : Sub Msg
subscriptions =
    let
        event name payload =
            GotWS (name ++ payload)
    in
    Sub.batch
        [ WebSocket.incomingWSMessage GotWS
        , WebSocket.onWSOffline (event "Offline")
        , WebSocket.onWSOnline (event "Online")
        , WebSocket.onWSSync (event "Sync")
        , WebSocket.replyWSMessage GotWS
        ]

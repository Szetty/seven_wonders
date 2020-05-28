module Pages.Game exposing (..)

import Common.Logger as Logger
import Common.Route as Route exposing (Route(..))
import Common.Session as Session exposing (Session(..), UserInfo, getNavKey)
import Common.WebStorage as WebStorage
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http exposing (Body)
import Image exposing (Image)
import Networking.WebSocket as WebSocket
import Services.GameService as GameService
import Services.LogoutService as LogoutService


type Msg
    = Ping
    | GotPong (Result Http.Error String)
    | GotImage (Result Http.Error (Maybe Image))
    | InitWebSocket
    | GotGameEvent String
    | Logout
    | GotLogout LogoutService.Msg


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
    , GameService.startWebSocket session
    )


toSession : Model -> Session
toSession { session } =
    session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- TODO needed in order for port to be created, temporary
        InitWebSocket ->
            if model.text == "INIT_WS" then
                let
                    cmd =
                        Cmd.batch
                            [ WebSocket.sendWSMessage ""
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

        GotGameEvent r ->
            ( model, Logger.log "Game EVENT" r )

        Logout ->
            case Session.getUserInfo model.session of
                Just userInfo ->
                    ( model, Cmd.map GotLogout (LogoutService.logout userInfo) )

                Nothing ->
                    ( model, Cmd.none )

        GotLogout logoutResponse ->
            case LogoutService.tryExtractResponse logoutResponse of
                ( Ok _, cmd ) ->
                    let
                        session =
                            Guest (getNavKey model.session)

                        cmdBatch =
                            Cmd.batch
                                [ Cmd.map GotLogout cmd
                                , WebStorage.deleteItem "userInfo"
                                , Route.replaceUrl (getNavKey model.session) Login
                                ]
                    in
                    ( { model | session = session }, cmdBatch )

                ( Err _, cmd ) ->
                    ( model, Cmd.map GotLogout cmd )


view : Model -> List (Html Msg)
view model =
    [ div [ class "mt-3" ]
        [ button [ onClick Ping, class "btn btn-primary" ] [ text "PING" ]
        , div [] [ text model.text ]
        , div [] []

        --, img [ src "%PUBLIC_URL%/wonders/alexandriaA.png" ] []
        , button [ onClick Logout, class "btn btn-outline-dark" ] [ text "Logout" ]
        ]
    ]


ping : Cmd Msg
ping =
    Http.get
        { url = "/api/ping"
        , expect = Http.expectString GotPong
        }


subscriptions =
    GameService.subscriptions GotGameEvent

module Pages.Game exposing (..)

import Common.Logger as Logger
import Common.Session exposing (Session(..), UserInfo)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Http exposing (Body)
import Image exposing (Image)
import Networking.WebSocket as WebSocket
import Pages.Header as Header exposing (Msg(..))
import Services.GameService as GameService


type Msg
    = HeaderEvent Header.Msg
    | Ping
    | GotPong (Result Http.Error String)
    | GotImage (Result Http.Error (Maybe Image))
    | InitWebSocket
    | GotGameEvent String


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
    , Cmd.batch [ Cmd.map HeaderEvent <| Header.init session, GameService.startWebSocket session ]
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
            case r of
                "Error" ->
                    ( model, Cmd.map HeaderEvent <| Header.checkTokenExpiration (toSession model) )

                _ ->
                    ( model, Logger.log "Game EVENT" r )

        HeaderEvent headerMsg ->
            let
                ( session, cmd ) =
                    Header.update headerMsg model.session
            in
            ( { model | session = session }, Cmd.map HeaderEvent cmd )


view : Model -> List (Html Msg)
view model =
    [ div [ class "page-holder bg-cove", style "background-image" "url('%PUBLIC_URL%/paper.jpg')" ]
        [ Html.map HeaderEvent <| Header.view model.session
        , button [ onClick Ping, class "btn btn-primary" ] [ text "PING" ]
        , div [] [ text model.text ]
        , div [] []
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

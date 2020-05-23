module Pages.Game exposing (..)

import Common.Session exposing (Session)
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Http
import Image exposing (Image)


type Msg
    = Ping
    | GotPong (Result Http.Error String)
    | GotImage (Result Http.Error (Maybe Image))


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
    , Cmd.none
    )


toSession : Model -> Session
toSession { session } =
    session


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

module Pages.Ping exposing (..)

import Common.Session exposing (Session(..), UserInfo)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Http exposing (Body)
import Pages.Header as Header


type Msg
    = HeaderEvent Header.Msg
    | Ping
    | GotPong (Result Http.Error String)


type alias Model =
    { session : Session
    , text : String
    }


init : Session -> ( Model, Cmd Msg )
init session =
    update Ping
        { session = session
        , text = ""
        }


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
        , div [] [ text model.text ]
        ]
    ]


ping : Cmd Msg
ping =
    Http.get
        { url = "/api/ping"
        , expect = Http.expectString GotPong
        }

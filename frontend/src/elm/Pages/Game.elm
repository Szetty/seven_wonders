module Pages.Game exposing (..)

import Common.Logger as Logger
import Common.Session exposing (Session(..), UserInfo)
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Pages.Header as Header
import Services.GameService as GameService


type Msg
    = HeaderEvent Header.Msg
    | GotGameEvent String


type alias Model =
    { session : Session
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      }
    , GameService.initGame session
    )


toSession : Model -> Session
toSession { session } =
    session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotGameEvent r ->
            case r of
                "Offline" ->
                    ( model, Cmd.map HeaderEvent <| Header.checkToken <| toSession model )

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
        ]
    ]


subscriptions =
    GameService.subscriptions GotGameEvent

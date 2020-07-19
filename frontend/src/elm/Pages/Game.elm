module Pages.Game exposing (..)

import Common.Logger as Logger
import Common.Session exposing (Session(..), UserInfo)
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Services.GameService as GameService
import Views.Header as Header


type Msg
    = HeaderEvent Header.Msg
    | GotGameEvent GameService.Message


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
                GameService.Offline ->
                    ( model
                    , Cmd.batch
                        [ Cmd.map HeaderEvent <| Header.checkToken <| toSession model
                        , Logger.log "Game EVENT" "Offline"
                        ]
                    )

                GameService.Online ->
                    ( model, Logger.log "Game EVENT" "Online" )

                GameService.Sync ->
                    ( model, Logger.log "Game EVENT" "Sync" )

                GameService.Error error ->
                    ( model, Logger.log "Game EVENT" ("Error" ++ error) )

                GameService.Incoming body ->
                    ( model, Logger.log "Game EVENT" ("Incoming" ++ body) )

                GameService.Reply body ->
                    ( model, Logger.log "Game EVENT" ("Reply" ++ body) )

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

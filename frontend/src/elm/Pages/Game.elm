module Pages.Game exposing (..)

import Common.Logger as Logger
import Common.Session exposing (Session(..))
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Html.Events exposing (on)
import Pages.Game.Playground as P
import Services.GameService as GameService
import Views.Header as Header
import Json.Decode as JsonDecode


type Msg
    = HeaderEvent Header.Msg
    | GotGameEvent GameService.Message
    | PlaygroundEvent P.Msg


type alias GameState =
    Bool


type alias Model =
    { session : Session
    , gameState : P.Game GameState
    }


init : Session -> ( Model, Cmd Msg )
init session =
    let
        ( gameState, gameCmd ) =
            P.initGame False
    in
    ( { session = session
      , gameState = gameState
      }
    , Cmd.batch
        [ GameService.initGame session
        , Cmd.map PlaygroundEvent gameCmd
        ]
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

        -- PlaygroundEvent (P.MouseWheel deltaY) ->
        --     (model, Logger.log "deltaY: " (String.fromFloat deltaY))

        PlaygroundEvent playgroundMsg ->
            let
                ( gameState, cmd ) =
                    P.updateGame updateGameState playgroundMsg model.gameState
            in
            ( { model | gameState = gameState }, cmd )


updateGameState _ model =
    model


view : Model -> List (Html Msg)
view model =
    [ div [ class "page-holder bg-cove", style "background-image" "url('%PUBLIC_URL%/paper.jpg')" ]
        [ Html.map HeaderEvent <| Header.view model.session
        , div [] [ Html.map PlaygroundEvent <| P.viewGame viewGameState model.gameState ]
        ]
    ]


type alias ViewContext =
    { screen_width : P.Number
    , screen_height : P.Number
    , card_width : P.Number
    , card_height : P.Number
    , wonder_width : P.Number
    , wonder_height : P.Number
    }


viewGameState : P.Computer -> GameState -> List P.Shape
viewGameState computer _ =
    let
        screen_width =
            computer.screen.width

        card_width =
            screen_width / 20

        wonder_width =
            screen_width / 4

        view_context =
            { screen_width = screen_width
            , screen_height = computer.screen.height
            , card_width = card_width
            , card_height = card_width * 1.5
            , wonder_width = wonder_width
            , wonder_height = wonder_width * 0.3125
            }

        cards_in_hand_shapes =
            List.repeat 9 "altar"
                |> List.map (card_image card_width view_context.card_height)
                |> List.map (List.repeat 1)
                |> vertically_align_shapes view_context 10
                |> List.concat

        self_wonder_shapes =
            view_wonder view_context mock_wonder
    in
    List.concat
        [ cards_in_hand_shapes
            |> List.map (P.move 0 (-view_context.card_height / 2))
        , self_wonder_shapes
            |> List.map (P.move 0 (-view_context.card_height * 1.5 - 10))
        ]
        |> List.map (P.move 0 (view_context.screen_height / 2))

view_wonder view_context wonder =
    let
        { screen_width, screen_height, card_width, card_height, wonder_height, wonder_width } =
            view_context

        build_card =
            card_image card_width card_height

        wonder_shape =
            wonder_image wonder_width wonder_height wonder.name

        wonder_card_shapes =
            wonder.stage_cards
                |> List.map build_card
                |> List.map (List.repeat 1)
                |> vertically_align_shapes view_context (wonder_width / 10)
                |> List.concat
                |> List.map (P.move 0 (-wonder_height / 2))

        built_card_mapper idx =
            P.move (card_width * toFloat idx / 5) (-card_height * toFloat idx / 4)

        built_card_shapes =
            wonder.built_cards
                |> List.map (List.map build_card)
                |> List.map (List.indexedMap built_card_mapper)
                |> vertically_align_shapes view_context (card_width * 0.8)
                |> List.concat
                |> List.map (P.move 0 (-wonder_height / 2 - card_height - 10))
    in
    wonder_card_shapes ++ [ wonder_shape ] ++ built_card_shapes


vertically_align_shapes : ViewContext -> P.Number -> List (List P.Shape) -> List (List P.Shape)
vertically_align_shapes { card_width, card_height } gap shape_groups =
    let
        len =
            List.length shape_groups

        mapper idx shapes =
            shapes
                |> List.map (P.move ((card_width + gap) * toFloat (idx - len // 2)) 0)
    in
    List.indexedMap mapper shape_groups


card_image : P.Number -> P.Number -> String -> P.Shape
card_image w h name =
    P.image w h ("%PUBLIC_URL%/cards/" ++ name ++ ".png")


wonder_image : P.Number -> P.Number -> String -> P.Shape
wonder_image w h name =
    P.image w h ("%PUBLIC_URL%/wonders/" ++ name ++ ".png")


subscriptions =
    GameService.subscriptions GotGameEvent

type alias Wonder =
    {   name : String
    ,   cards_in_hand : List String
    ,   wonder_cards : List String
    ,   built_cards : List (List String)
    }

mock_wonder = 
    {   name = "alexandriaA"
    ,   stage_cards = ["age1", "age2", "age3"]
    ,   built_cards =
            [ [ "baths", "altar", "theater", "temple", "statue", "palace", "townhall", "pantheon", "gardens" ]
            , [ "tavern", "marketplace", "forum", "haven", "vineyard", "bazar", "lighthouse", "vineyard", "marketplace" ]
            , [ "loom", "glassworks", "press" ]
            , [ "stonepit", "claypit", "mine", "orevein", "excavation", "sawmill", "quarry", "foundry", "claypool" ]
            , [ "spiesguild", "workersguild", "tradersguild", "buildersguild", "magistratesguild", "craftsmensguild", "scientistsguild" ]
            , [ "stockade", "barracks", "walls", "stables", "circus", "arsenal", "fortifications", "guardtower", "archeryrange" ]
            , [ "apothecary", "dispensary", "lodge", "library", "school", "academy", "study", "university", "workshop" ]
            ]
    }
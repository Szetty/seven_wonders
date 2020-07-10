module Common.Route exposing (..)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, map, oneOf, s, string)


type Route
    = Root
    | Game
    | Lobby String
    | Login
    | Ping


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Root Parser.top
        , map Login (s "login")
        , map Lobby (s "lobby" </> string)
        , map Game (s "game")
        , map Ping (s "ping")
        ]



-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl =
    Parser.parse parser



-- INTERNAL


routeToString : Route -> String
routeToString page =
    "/" ++ String.join "/" (routeToPieces page)


routeToPieces : Route -> List String
routeToPieces page =
    case page of
        Root ->
            []

        Game ->
            [ "game" ]

        Login ->
            [ "login" ]

        Ping ->
            [ "ping" ]

        Lobby gameID ->
            [ "lobby", gameID ]

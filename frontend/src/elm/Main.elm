module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Common.Domain exposing (SessionData, decodeSessionData)
import Common.Route as Route exposing (Route)
import Common.Session exposing (Session(..), getNavKey, initSession)
import Html exposing (div, text)
import Pages.Game as Game
import Pages.Lobby as Lobby
import Pages.Login as Login
import Pages.Ping as Ping
import Url exposing (Url)


type Model
    = Game Game.Model
    | Lobby Lobby.Model
    | Login Login.Model
    | Ping Ping.Model
    | NotFound Session
    | Redirect Session


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotGameMsg Game.Msg
    | GotLobbyMsg Lobby.Msg
    | GotLoginMsg Login.Msg
    | GotPingMsg Ping.Msg


init : String -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    changeRouteTo (Route.fromUrl url) (initSession navKey (decodeSessionData flags))


changeRouteTo : Maybe Route -> Session -> ( Model, Cmd Msg )
changeRouteTo maybeRoute session =
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Root ->
            case session of
                Guest _ ->
                    ( Redirect session, Route.replaceUrl (getNavKey session) Route.Login )

                LoggedIn _ sessionData ->
                    ( Redirect session, Route.replaceUrl (getNavKey session) (Route.Lobby sessionData.userInfo.gameID) )

        Just Route.Game ->
            case session of
                Guest _ ->
                    ( Redirect session, Route.replaceUrl (getNavKey session) Route.Login )

                LoggedIn _ _ ->
                    Game.init session
                        |> updateWith Game GotGameMsg

        Just (Route.Lobby gameID) ->
            case session of
                Guest _ ->
                    ( Redirect session, Route.replaceUrl (getNavKey session) Route.Login )

                LoggedIn _ _ ->
                    Lobby.init session gameID
                        |> updateWith Lobby GotLobbyMsg

        Just Route.Login ->
            case session of
                Guest _ ->
                    Login.init session
                        |> updateWith Login GotLoginMsg

                LoggedIn _ sessionData ->
                    ( Redirect session, Route.replaceUrl (getNavKey session) (Route.Lobby sessionData.userInfo.gameID) )

        Just Route.Ping ->
            case session of
                Guest _ ->
                    Login.init session
                        |> updateWith Login GotLoginMsg

                LoggedIn _ _ ->
                    Ping.init session
                        |> updateWith Ping GotPingMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl (getNavKey (toSession model)) (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) (toSession model)

        ( GotLoginMsg subMsg, Login login ) ->
            Login.update subMsg login
                |> updateWith Login GotLoginMsg

        ( GotLobbyMsg subMsg, Lobby lobby ) ->
            Lobby.update subMsg lobby
                |> updateWith Lobby GotLobbyMsg

        ( GotGameMsg subMsg, Game game ) ->
            Game.update subMsg game
                |> updateWith Game GotGameMsg

        ( GotPingMsg subMsg, Ping ping ) ->
            Ping.update subMsg ping
                |> updateWith Ping GotPingMsg

        ( _, _ ) ->
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


toSession : Model -> Session
toSession page =
    case page of
        NotFound session ->
            session

        Game game ->
            Game.toSession game

        Lobby lobby ->
            Lobby.toSession lobby

        Login login ->
            Login.toSession login

        Redirect session ->
            session

        Ping ping ->
            Ping.toSession ping


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Game _ ->
            Sub.map GotGameMsg Game.subscriptions

        Lobby _ ->
            Sub.map GotLobbyMsg Lobby.subscriptions

        _ ->
            Sub.none


view : Model -> Document Msg
view model =
    let
        viewPage body toMsg =
            { title = "7 Wonders"
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        NotFound _ ->
            viewPage notFoundView identity

        Game game ->
            viewPage (Game.view game) GotGameMsg

        Lobby lobby ->
            viewPage (Lobby.view lobby) GotLobbyMsg

        Login login ->
            viewPage (Login.view login) GotLoginMsg

        Redirect _ ->
            viewPage notFoundView identity

        Ping ping ->
            viewPage (Ping.view ping) GotPingMsg


notFoundView =
    [ div [] [ text "Page was not found" ] ]


main : Program String Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }

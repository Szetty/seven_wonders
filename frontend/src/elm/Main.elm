module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Common.Route as Route exposing (Route)
import Common.Session exposing (Session(..), UserInfo, getNavKey, initSession)
import Html exposing (div, text)
import Pages.Game as Game
import Pages.Login as Login
import Url exposing (Url)


type Model
    = Game Game.Model
    | Login Login.Model
    | NotFound Session
    | Redirect Session


type alias Flags =
    { userInfo : Maybe UserInfo }


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotGameMsg Game.Msg
    | GotLoginMsg Login.Msg


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    changeRouteTo (Route.fromUrl url) (initSession navKey flags.userInfo)


changeRouteTo : Maybe Route -> Session -> ( Model, Cmd Msg )
changeRouteTo maybeRoute session =
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Root ->
            case session of
                Guest _ ->
                    ( Redirect session, Route.replaceUrl (getNavKey session) Route.Login )

                LoggedIn _ _ ->
                    ( Redirect session, Route.replaceUrl (getNavKey session) Route.Game )

        Just Route.Game ->
            case session of
                Guest _ ->
                    ( Redirect session, Route.replaceUrl (getNavKey session) Route.Login )

                LoggedIn _ _ ->
                    Game.init session
                        |> updateWith Game GotGameMsg

        Just Route.Login ->
            case session of
                Guest _ ->
                    Login.init session
                        |> updateWith Login GotLoginMsg

                LoggedIn _ _ ->
                    ( Redirect session, Route.replaceUrl (getNavKey session) Route.Game )


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

        ( GotGameMsg subMsg, Game game ) ->
            Game.update subMsg game
                |> updateWith Game GotGameMsg

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

        Login login ->
            Login.toSession login

        Redirect session ->
            session


subscriptions : Model -> Sub Msg
subscriptions _ =
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

        Login login ->
            viewPage (Login.view login) GotLoginMsg

        Redirect _ ->
            viewPage notFoundView identity


notFoundView =
    [ div [] [ text "Page was not found" ] ]


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }

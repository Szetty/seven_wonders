module Views.Header exposing (..)

import Common.Error exposing (ErrorType(..))
import Common.Route as Route exposing (Route(..))
import Common.Session exposing (Session(..), getCurrentUsername, getNavKey, getUserInfo)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Services.LogoutService as LogoutService
import Services.TokenService as TokenService
import Services.WebSocketService as WebSocketService
import Services.WebStorageService as WebStorageService


type Msg
    = OnTokenCheck TokenService.Msg
    | Logout
    | GotLogout LogoutService.Msg


init : Session -> Cmd Msg
init =
    checkToken


checkToken : Session -> Cmd Msg
checkToken session =
    case getUserInfo session of
        Just userInfo ->
            Cmd.map OnTokenCheck <| TokenService.checkToken { token = userInfo.userToken }

        Nothing ->
            Cmd.none


update : Msg -> Session -> ( Session, Cmd Msg )
update msg session =
    case msg of
        Logout ->
            case getUserInfo session of
                Just userInfo ->
                    let
                        ( newSession, clearCmd ) =
                            clearSession session
                    in
                    ( newSession, Cmd.batch [ Cmd.map GotLogout (LogoutService.logout userInfo), clearCmd ] )

                Nothing ->
                    ( Guest (getNavKey session), Cmd.none )

        GotLogout logoutRes ->
            case LogoutService.tryExtractResponse logoutRes of
                ( _, cmd ) ->
                    ( session, Cmd.map GotLogout cmd )

        OnTokenCheck tokenCheckRes ->
            case TokenService.tryExtractResponse tokenCheckRes of
                ( Ok _, cmd ) ->
                    ( session, Cmd.map OnTokenCheck cmd )

                ( Err error, cmd ) ->
                    if error.kind == Unauthorized || error.kind == InvalidUser then
                        let
                            ( newSession, clearCmd ) =
                                clearSession session
                        in
                        ( newSession, Cmd.batch [ Cmd.map OnTokenCheck cmd, clearCmd ] )

                    else
                        ( session, Cmd.map OnTokenCheck cmd )


clearSession : Session -> ( Session, Cmd Msg )
clearSession session =
    ( Guest (getNavKey session)
    , Cmd.batch
        [ WebStorageService.deleteSessionData
        , WebSocketService.close
        , Route.replaceUrl (getNavKey session) Login
        ]
    )


view : Session -> Html Msg
view session =
    div [ class "header static-top p-3 mb-5" ]
        [ div [ class "d-flex justify-content-between" ]
            [ div [ class "mt-1 btn-like-text" ] [ text (getCurrentUsername session) ]
            , button [ onClick Logout, class "btn btn-dark" ] [ text "Logout" ]
            ]
        ]

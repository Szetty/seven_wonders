module Pages.Header exposing (..)

import Common.Route as Route exposing (Route(..))
import Common.Session exposing (Session(..), getName, getNavKey, getUserInfo)
import Common.WebStorage as WebStorage
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Jwt exposing (JwtError)
import Networking.WebSocket as WebSocket
import Services.LogoutService as LogoutService
import Task


type Msg
    = OnTokenExpiryCheck (Result JwtError Bool)
    | Logout
    | GotLogout LogoutService.Msg


init : Session -> Cmd Msg
init =
    checkTokenExpiration


checkTokenExpiration : Session -> Cmd Msg
checkTokenExpiration session =
    case getUserInfo session of
        Just userInfo ->
            Jwt.checkTokenExpiry userInfo.userToken
                |> Task.attempt OnTokenExpiryCheck

        Nothing ->
            Cmd.none


update : Msg -> Session -> ( Session, Cmd Msg )
update msg session =
    case msg of
        OnTokenExpiryCheck res ->
            case res of
                Ok False ->
                    ( session, Cmd.none )

                _ ->
                    update Logout session

        Logout ->
            case getUserInfo session of
                Just userInfo ->
                    ( session, Cmd.map GotLogout (LogoutService.logout userInfo) )

                Nothing ->
                    ( session, Cmd.none )

        GotLogout logoutResponse ->
            case LogoutService.tryExtractResponse logoutResponse of
                ( Ok _, cmd ) ->
                    let
                        newSession =
                            Guest (getNavKey session)

                        cmdBatch =
                            Cmd.batch
                                [ Cmd.map GotLogout cmd
                                , WebStorage.deleteItem "userInfo"
                                , WebSocket.closeWebSocket ""
                                , Route.replaceUrl (getNavKey session) Login
                                ]
                    in
                    ( newSession, cmdBatch )

                ( Err _, cmd ) ->
                    ( session, Cmd.map GotLogout cmd )


view : Session -> Html Msg
view session =
    let
        name =
            case getName session of
                Just n ->
                    n

                Nothing ->
                    "Anonymous"
    in
    div [ class "header static-top p-3 mb-5" ]
        [ div [ class "d-flex justify-content-between" ]
            [ div [ class "mt-1 btn-like-text" ] [ text name ]
            , button [ onClick Logout, class "btn btn-dark" ] [ text "Logout" ]
            ]
        ]

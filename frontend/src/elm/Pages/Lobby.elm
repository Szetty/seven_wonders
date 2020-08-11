module Pages.Lobby exposing (..)

import Common.Domain exposing (AcceptData(..), Notification, SavedNotification)
import Common.Logger as Logger
import Common.Route as Route exposing (Route(..))
import Common.Session exposing (Session(..), getCurrentUsername, getNavKey, getSavedNotifications)
import Dict exposing (Dict, size)
import Html exposing (Attribute, Html, button, div, option, select, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, colspan, disabled)
import Html.Events exposing (onClick, onInput)
import Services.LobbyService as LobbyService exposing (MessageBody(..))
import Services.WebStorageService as WebStorageService
import Views.Header as Header
import Views.Notification as Notification exposing (addApproveNotification)


type Msg
    = NotificationEvent Notification.Msg
    | HeaderEvent Header.Msg
    | Invite
    | Uninvite String
    | SetToInviteUsername String
    | GotLobbyMessage LobbyService.Message


type alias Model =
    { session : Session
    , gameID : String
    , toInviteUsername : String
    , onlineUsernames : List String
    , invitedUsernames : Dict String Bool
    , notificationModel : Notification.Model
    }


init : Session -> String -> ( Model, Cmd Msg )
init session gameID =
    let
        ( notificationModel, notificationCmd ) =
            Notification.init session
    in
    ( { session = session
      , gameID = gameID
      , toInviteUsername = ""
      , onlineUsernames = []
      , invitedUsernames = Dict.fromList []
      , notificationModel = notificationModel
      }
    , Cmd.batch
        [ LobbyService.initGameLobby session
        , Cmd.map NotificationEvent notificationCmd
        ]
    )


toSession : Model -> Session
toSession { session } =
    session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HeaderEvent headerMsg ->
            let
                ( session, cmd ) =
                    Header.update headerMsg model.session
            in
            ( { model | session = session }, Cmd.map HeaderEvent cmd )

        NotificationEvent notificationMsg ->
            let
                ( notification, cmd1 ) =
                    Notification.update notificationMsg model.notificationModel

                cmd2 =
                    case notificationMsg of
                        Notification.OnAcceptFromNotification _ acceptData ->
                            case acceptData of
                                String value ->
                                    Route.replaceUrl (getNavKey model.session) (Lobby value)

                        _ ->
                            Cmd.none

                newSession =
                    Notification.deleteApproveNotification model.session model.notificationModel.currentId
            in
            ( { model | notificationModel = notification, session = newSession }
            , Cmd.batch
                [ Cmd.map NotificationEvent cmd1
                , cmd2
                ]
            )

        Invite ->
            ( model
            , LobbyService.inviteUser model.toInviteUsername
            )

        Uninvite toInvite ->
            ( model
            , LobbyService.uninviteUser toInvite
            )

        SetToInviteUsername username ->
            let
                name =
                    username
            in
            ( { model | toInviteUsername = name }, Cmd.none )

        GotLobbyMessage message ->
            case message of
                LobbyService.Offline ->
                    ( model
                    , Cmd.batch
                        [ Cmd.map HeaderEvent <| Header.checkToken <| toSession model
                        , Logger.log "Lobby WS EVENT" "Offline"
                        ]
                    )

                LobbyService.Online ->
                    ( model, Logger.log "Lobby WS EVENT" "Online" )

                LobbyService.Sync ->
                    ( model
                    , Cmd.batch
                        [ Logger.log "Lobby WS EVENT" "Sync"
                        , LobbyService.getOnlineUsers
                        , LobbyService.getInvitedUsers
                        ]
                    )

                LobbyService.Reply messageBody ->
                    case messageBody of
                        OnlineUsersReply onlineUsers ->
                            ( { model | onlineUsernames = onlineUsers }, Cmd.none )

                        InvitedUsersReply invitedUsers ->
                            let
                                toTupleList =
                                    List.map (\invitedUser -> ( invitedUser.name, invitedUser.connected ))
                            in
                            ( { model | invitedUsernames = Dict.fromList <| toTupleList invitedUsers }, Cmd.none )

                        InviteUserReply username ->
                            let
                                updatedInvitedUsernames =
                                    Dict.insert username False model.invitedUsernames
                            in
                            ( { model | invitedUsernames = updatedInvitedUsernames }
                            , Cmd.none
                            )

                        UninviteUserReply username ->
                            let
                                updatedInvitedUsernames =
                                    Dict.remove username model.invitedUsernames
                            in
                            ( { model | invitedUsernames = updatedInvitedUsernames }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                LobbyService.Incoming messageBody ->
                    let
                        currentUsername =
                            getCurrentUsername model.session
                    in
                    case messageBody of
                        UserGotOnline username ->
                            let
                                notificationToBeAdded =
                                    Notification.simpleNotification ("User " ++ username ++ " got online!")
                            in
                            ( if username /= currentUsername then
                                { model
                                    | onlineUsernames = model.onlineUsernames ++ [ username ]
                                    , notificationModel =
                                        Notification.addNotification model.notificationModel notificationToBeAdded
                                }

                              else
                                model
                            , Cmd.none
                            )

                        UserGotOffline username ->
                            let
                                notificationToBeAdded =
                                    Notification.simpleNotification ("User " ++ username ++ " got offline!")
                            in
                            ( if username /= currentUsername then
                                { model
                                    | onlineUsernames = List.filter (\u -> not (u == username)) model.onlineUsernames
                                    , notificationModel =
                                        Notification.addNotification model.notificationModel notificationToBeAdded
                                }

                              else
                                model
                            , Cmd.none
                            )

                        GotInvite user ->
                            let
                                notificationToBeAdded =
                                    Notification.approveNotification ("You are expected on table " ++ user.name) (String user.gameID)

                                savedNotification =
                                    { id = model.notificationModel.currentId, message = notificationToBeAdded.message, acceptData = String user.gameID }

                                newSession =
                                    addApproveNotification model.session savedNotification
                            in
                            ( { model
                                | notificationModel =
                                    Notification.addNotification model.notificationModel
                                        notificationToBeAdded
                                , session = newSession
                              }
                            , WebStorageService.saveNotification (getSavedNotifications newSession)
                            )

                        _ ->
                            ( model, Cmd.none )

                LobbyService.Error errorMessage ->
                    ( model, Logger.log "Lobby WS Error" errorMessage )


view : Model -> List (Html Msg)
view model =
    let
        currentUsername =
            getCurrentUsername model.session

        numberOfPlayers =
            7
    in
    [ div [] [ Html.map NotificationEvent <| Notification.view model.notificationModel ]
    , div []
        [ Html.map HeaderEvent <| Header.view model.session
        ]
    , div [ class "" ]
        [ select [ onInput SetToInviteUsername, class "custom-select-md mr-4 btn btn-md btn-primary" ] <|
            [ option [] [ text "Select username" ] ]
                ++ List.map (\onlineUsername -> option [] [ text onlineUsername ])
                    (List.filter
                        (\onlineUsername ->
                            (not <| Dict.member onlineUsername model.invitedUsernames) && onlineUsername /= currentUsername
                        )
                        model.onlineUsernames
                    )
        , button
            [ onClick Invite
            , class "btn btn-dark"
            , if model.toInviteUsername == "Select username" || model.toInviteUsername == "" then
                disabled True

              else
                disabled False
            ]
            [ text "Invite" ]
        ]
    , div [ class "table-responsive" ]
        [ table [ class "table table-md table-light mt-4" ]
            [ thead [ class "thead-dark" ] [ tr [] [ th [] [ text "Username" ], th [] [ text "Delete" ] ] ]
            , tbody []
                ([ tr [] [ td [ class "align-middle" ] [ text <| currentUsername ], td [] [] ] ]
                    ++ (Dict.toList model.invitedUsernames
                            |> List.map
                                (\( invitedUsername, isConnected ) ->
                                    tr
                                        [ if not isConnected then
                                            class "not-connected-row"

                                          else
                                            class ""
                                        ]
                                        [ td [ class "align-middle" ] [ text invitedUsername ]
                                        , td [] [ button [ onClick (Uninvite invitedUsername), class "btn btn-dark" ] [ text "x" ] ]
                                        ]
                                )
                       )
                    ++ List.repeat (numberOfPlayers - size model.invitedUsernames - 1) (tr [] [ td [ colspan 4 ] [ text "FREE" ] ])
                )
            ]
        ]
    , button [ class "btn btn-dark mt-4" ] [ text "Start" ]
    ]


subscriptions =
    LobbyService.subscriptions GotLobbyMessage

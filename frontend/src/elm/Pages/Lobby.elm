module Pages.Lobby exposing (..)

import Common.Domain exposing (InvitedUser, Metadata(..), Notification, NotificationType(..), SavedNotification)
import Common.Logger as Logger
import Common.Route as Route exposing (Route(..))
import Common.Session exposing (Session(..), getCurrentUsername, getGameID, getNavKey, getSavedNotifications)
import Dict exposing (Dict, size)
import Html exposing (Attribute, Html, button, div, i, option, select, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, colspan, disabled)
import Html.Events exposing (onClick, onInput)
import Services.LobbyService as LobbyService exposing (MessageBody(..))
import Services.WebStorageService as WebStorageService
import Views.Header as Header
import Views.Notification as Notification exposing (addApproveNotification, deleteApproveNotificationByGameId, deleteNotificationByGameId)


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
    , invitedUsers : Dict String InvitedUser
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
      , invitedUsers = Dict.fromList []
      , notificationModel = notificationModel
      }
    , Cmd.batch
        [ LobbyService.initGameLobby session gameID
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
                ( newSession, notification, updateResultCmd ) =
                    Notification.update notificationMsg model.notificationModel model.session

                notificationActionCmd =
                    case notificationMsg of
                        Notification.NotificationAccepted _ metadata ->
                            case metadata of
                                String gameID ->
                                    Cmd.batch
                                        [ LobbyService.closeLobby
                                        , Route.replaceUrl (getNavKey newSession) (Lobby gameID)
                                        ]

                        Notification.NotificationDeclined _ metadata ->
                            case metadata of
                                String gameID ->
                                    LobbyService.declineInvitation gameID

                        _ ->
                            Cmd.none

                mapNotificationCmd =
                    Cmd.map NotificationEvent updateResultCmd
            in
            ( { model | notificationModel = notification, session = newSession }
            , Cmd.batch
                [ mapNotificationCmd
                , notificationActionCmd
                ]
            )

        Invite ->
            ( model
            , LobbyService.inviteUser model.toInviteUsername
            )

        Uninvite toInvite ->
            let
                updatedInvitedUsernames =
                    Dict.remove toInvite model.invitedUsers
            in
            ( { model | invitedUsers = updatedInvitedUsernames }
            , LobbyService.uninviteUser toInvite
            )

        SetToInviteUsername username ->
            ( { model | toInviteUsername = username }, Cmd.none )

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
                                    List.map (\invitedUser -> ( invitedUser.name, InvitedUser invitedUser.connected invitedUser.leader ))
                            in
                            ( { model | invitedUsers = Dict.fromList <| toTupleList invitedUsers }, Cmd.none )

                        InviteUserReply username ->
                            let
                                updatedInvitedUsernames =
                                    Dict.insert username (InvitedUser False False) model.invitedUsers
                            in
                            ( { model | invitedUsers = updatedInvitedUsernames, toInviteUsername = "" }
                            , Cmd.none
                            )

                        UninviteUserReply ->
                            ( model
                            , Cmd.none
                            )

                        DeclineInvitationReply ->
                            ( model
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
                        Connected username ->
                            let
                                updateConnectedUser maybeValue =
                                    case maybeValue of
                                        Nothing ->
                                            Just { connected = True, leader = False }

                                        Just invitedUser ->
                                            Just { invitedUser | connected = True }

                                updatedInvitedUsers =
                                    Dict.update username updateConnectedUser model.invitedUsers
                            in
                            ( { model | invitedUsers = updatedInvitedUsers }, Cmd.none )

                        Disconnected username ->
                            let
                                updatedInvitedUsers =
                                    Dict.update username updateInvitedUser model.invitedUsers
                            in
                            ( { model | invitedUsers = updatedInvitedUsers }, Cmd.none )

                        DeclinedInvitation username ->
                            let
                                notificationToBeAdded =
                                    Notification.simpleNotification ("User " ++ username ++ " declined your invitation!")

                                updatedInvitedUsernames =
                                    Dict.remove username model.invitedUsers

                                isSameUsername =
                                    username == currentUsername
                            in
                            ( if not isSameUsername then
                                { model
                                    | notificationModel =
                                        Notification.addNotification model.notificationModel notificationToBeAdded
                                    , invitedUsers = updatedInvitedUsernames
                                }

                              else
                                model
                            , Cmd.none
                            )

                        UserGotOnline username ->
                            let
                                notificationToBeAdded =
                                    Notification.simpleNotification ("User " ++ username ++ " got online!")

                                isSameUsername =
                                    username == currentUsername
                            in
                            ( if not isSameUsername then
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

                                updatedInvitedUsernames =
                                    Dict.update username updateInvitedUser model.invitedUsers

                                isSameUsername =
                                    username == currentUsername
                            in
                            ( if not isSameUsername then
                                { model
                                    | onlineUsernames = List.filter (\u -> not (u == username)) model.onlineUsernames
                                    , notificationModel =
                                        Notification.addNotification model.notificationModel notificationToBeAdded
                                    , invitedUsers = updatedInvitedUsernames
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
                                    Notification.addNotification model.notificationModel notificationToBeAdded
                                , session = newSession
                              }
                            , WebStorageService.saveNotifications (getSavedNotifications newSession)
                            )

                        GotUninvite gameID ->
                            let
                                newSession =
                                    deleteApproveNotificationByGameId model.session gameID

                                replaceUrlCmd =
                                    Route.replaceUrl (getNavKey newSession) (Lobby (getCurrentGameID model))

                                saveToWebstorageCmd =
                                    WebStorageService.saveNotifications (getSavedNotifications newSession)

                                checkGameIDCmd =
                                    if gameID == model.gameID then
                                        Cmd.batch [ LobbyService.closeLobby, replaceUrlCmd ]

                                    else
                                        saveToWebstorageCmd

                                oldNotificationModel =
                                    model.notificationModel

                                notificationModel =
                                    deleteNotificationByGameId model.notificationModel gameID
                            in
                            ( { model | notificationModel = { oldNotificationModel | notifications = notificationModel.notifications } }
                            , checkGameIDCmd
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

        invitedUsers =
            Dict.toList model.invitedUsers

        connectedUsers =
            Dict.toList model.invitedUsers |> List.filter (\( _, invitedUser ) -> invitedUser.connected)

        body =
            if model.gameID == getCurrentGameID model then
                div [] [ viewInvited model currentUsername, viewUsersTable model currentUsername invitedUsers ]

            else
                div [] [ viewUsersTable model currentUsername connectedUsers ]
    in
    [ viewTopPage model
    , body
    ]


viewTopPage : Model -> Html Msg
viewTopPage model =
    div []
        [ div [] [ Html.map NotificationEvent <| Notification.view model.notificationModel ]
        , div []
            [ Html.map HeaderEvent <| Header.view model.session
            ]
        ]


viewInvited : Model -> String -> Html Msg
viewInvited model currentUsername =
    let
        defaultOption =
            "Select username"

        toInviteUsernames =
            List.filter
                (\onlineUsername ->
                    (not <| Dict.member onlineUsername model.invitedUsers) && onlineUsername /= currentUsername
                )
                model.onlineUsernames
    in
    div
        []
        [ select [ onInput SetToInviteUsername, class "custom-select-md mr-4 btn btn-md btn-primary" ] <|
            [ option [] [ text defaultOption ] ]
                ++ List.map (\onlineUsername -> option [] [ text onlineUsername ]) toInviteUsernames
        , button
            [ onClick Invite
            , class "btn btn-dark"
            , if model.toInviteUsername == defaultOption || model.toInviteUsername == "" then
                disabled True

              else
                disabled False
            ]
            [ text "Invite" ]
        ]


viewUsersTable : Model -> String -> List ( String, InvitedUser ) -> Html Msg
viewUsersTable model currentUsername usernames =
    let
        numberOfPlayers =
            7

        isSameGameId =
            model.gameID == getCurrentGameID model

        showDeleteHeader =
            if isSameGameId then
                [ th [] [ text "Delete" ] ]

            else
                []

        showHeader =
            [ th [] [ text "Username" ] ] ++ showDeleteHeader

        showFreeSlots =
            List.repeat (numberOfPlayers - size model.invitedUsers) (tr [] [ td [ colspan 4 ] [ text "FREE" ] ])

        startButton =
            button
                [ class "btn btn-dark mt-4" ]
                [ text "Start" ]
    in
    div [ class "table-responsive" ]
        [ table
            [ class "table table-md table-light mt-4" ]
            [ thead [ class "thead-dark" ]
                [ tr [] showHeader ]
            , tbody []
                ((usernames
                    |> List.map
                        (\( invitedUsername, invitedUser ) ->
                            tr
                                [ if not invitedUser.connected then
                                    class "not-connected-row"

                                  else
                                    class ""
                                ]
                                ([ td [ class "align-middle" ]
                                    [ if invitedUsername == currentUsername then
                                        div [] [ i [ class "fas fa-angle-double-right mr-2" ] [], text invitedUsername ]

                                      else if invitedUser.leader then
                                        div [] [ i [ class "fas fa-crown mr-2" ] [], text invitedUsername ]

                                      else
                                        text invitedUsername
                                    ]
                                 ]
                                    ++ (if isSameGameId then
                                            [ td []
                                                [ if not (invitedUsername == currentUsername) then
                                                    button
                                                        [ onClick (Uninvite invitedUsername), class "btn btn-dark" ]
                                                        [ text "x" ]

                                                  else
                                                    text ""
                                                ]
                                            ]

                                        else
                                            []
                                       )
                                )
                        )
                 )
                    ++ showFreeSlots
                )
            ]
        , startButton
        ]


subscriptions =
    LobbyService.subscriptions GotLobbyMessage


getCurrentGameID : Model -> String
getCurrentGameID model =
    let
        maybeCurrentGameID =
            getGameID model.session

        currentGameID =
            case maybeCurrentGameID of
                Just value ->
                    value

                Nothing ->
                    ""
    in
    currentGameID


updateInvitedUser maybeValue =
    case maybeValue of
        Nothing ->
            Nothing

        Just invitedUser ->
            Just { invitedUser | connected = False }

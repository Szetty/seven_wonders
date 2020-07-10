module Pages.Lobby exposing (..)

import Common.Logger as Logger
import Common.Session exposing (Session(..), UserInfo, getCurrentUsername)
import Dict exposing (Dict, size)
import Html exposing (Attribute, Html, a, button, div, option, select, table, tbody, td, text, th, thead, tr)
import Html.Attributes as Attributes exposing (attribute, class, colspan, id, style, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Pages.Header as Header
import Services.LobbyService as LobbyService exposing (MessageBody(..))


type Msg
    = HeaderEvent Header.Msg
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
    }


init : Session -> String -> ( Model, Cmd Msg )
init session gameID =
    ( { session = session
      , gameID = gameID
      , toInviteUsername = ""
      , onlineUsernames = []
      , invitedUsernames = Dict.fromList []
      }
    , LobbyService.initGameLobby session
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
            ( { model | toInviteUsername = name }, Logger.log "" username )

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
                    case messageBody of
                        UserGotOnline username ->
                            ( { model | onlineUsernames = model.onlineUsernames ++ [ username ] }, Cmd.none )

                        UserGotOffline username ->
                            ( { model | onlineUsernames = List.filter (\u -> u == username) model.onlineUsernames }, Cmd.none )

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
    [ div []
        [ Html.map HeaderEvent <| Header.view model.session
        ]
    , div [ class "" ]
        [ select [ onInput SetToInviteUsername, class "custom-select-md mr-4 btn btn-lg btn-primary" ] <|
            [ option [] [ text "" ] ]
                ++ List.map (\onlineUsername -> option [] [ text onlineUsername ])
                    (List.filter
                        (\onlineUsername ->
                            (not <| Dict.member onlineUsername model.invitedUsernames) && onlineUsername /= currentUsername
                        )
                        model.onlineUsernames
                    )
        , button [ onClick Invite, class "btn btn-dark" ] [ text "Invite" ]
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

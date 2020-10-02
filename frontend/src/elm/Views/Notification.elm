module Views.Notification exposing (..)

import Common.Domain exposing (Metadata(..), Notification, NotificationType(..), NotificationWithId, SavedNotification, savedNotificationToNotificationWithId)
import Common.Logger as Logger
import Common.Session exposing (Session(..), getNavKey, getSavedNotifications)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, hidden)
import Html.Events exposing (onClick)
import Services.WebStorageService as WebStorageService


type Msg
    = NotificationAccepted Int Metadata
    | NotificationDeclined Int Metadata
    | NotificationDiscarded Int


type alias Model =
    { notifications : List NotificationWithId
    , currentId : Int
    }


init : Session -> ( Model, Cmd Msg )
init session =
    let
        savedNotificationsMaybe =
            getSavedNotifications session

        notifications =
            case savedNotificationsMaybe of
                Just savedNotifications ->
                    List.map savedNotificationToNotificationWithId savedNotifications

                Nothing ->
                    []
    in
    ( { notifications = notifications
      , currentId = computeCurrentId notifications
      }
    , Cmd.none
    )


update : Msg -> Model -> Session -> ( Session, Model, Cmd Msg )
update msg model session =
    case msg of
        NotificationAccepted id _ ->
            let
                newSession =
                    deleteApproveNotificationById session id

                cmd =
                    WebStorageService.saveNotifications (getSavedNotifications newSession)
            in
            ( newSession, deleteNotificationById model id, cmd )

        NotificationDeclined id _ ->
            ( session, deleteNotificationById model id, Cmd.none )

        NotificationDiscarded id ->
            ( session, deleteNotificationById model id, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ viewNotifications model.notifications
        ]


viewNotifications : List ( Int, Notification ) -> Html Msg
viewNotifications notifications =
    let
        isHidden =
            if List.isEmpty notifications then
                True

            else
                False

        notificationsHtml =
            notifications
                |> List.map viewNotification
                |> List.take 5
    in
    div [ class "text-light", hidden isHidden ] notificationsHtml


viewNotification : ( Int, Notification ) -> Html Msg
viewNotification ( id, notification ) =
    let
        buttons =
            case notification.notificationType of
                Simple ->
                    [ button [ onClick (NotificationDiscarded id), class "btn btn-dark btn-sm ml-2" ] [ text "Ok" ] ]

                Approve metadata ->
                    [ button [ onClick (NotificationAccepted id metadata), class "btn btn-primary btn-sm ml-2" ] [ text "Accept" ]
                    , button [ onClick (NotificationDeclined id metadata), class "btn btn-dark btn-sm ml-2" ] [ text "Decline" ]
                    ]
    in
    span [ class "d-block mb-1 border border-dark bg-info" ] <| text notification.message :: buttons


addNotification : Model -> Notification -> Model
addNotification model notification =
    let
        newNotifications =
            if List.length model.notifications >= 3 then
                case model.notifications of
                    n1 :: (n2 :: (( _, n3Notification ) :: ns)) ->
                        case n3Notification.notificationType of
                            Simple ->
                                n1 :: (n2 :: ns)

                            _ ->
                                model.notifications

                    _ ->
                        model.notifications

            else
                model.notifications
    in
    { model | notifications = ( model.currentId, notification ) :: newNotifications, currentId = model.currentId + 1 }


deleteNotificationById : Model -> Int -> Model
deleteNotificationById model idToDelete =
    let
        newNotifications =
            List.filter (\( id, _ ) -> not (id == idToDelete)) model.notifications
    in
    { model | notifications = newNotifications }


deleteNotificationByGameId : Model -> String -> Model
deleteNotificationByGameId model gameIdToDelete =
    let
        hasDiffGameID ( _, notification ) =
            case notification.notificationType of
                Approve accept ->
                    case accept of
                        String gID ->
                            not (gID == gameIdToDelete)

                _ ->
                    True

        newNotifications =
            List.filter hasDiffGameID model.notifications
    in
    { model | notifications = newNotifications }


addApproveNotification : Session -> SavedNotification -> Session
addApproveNotification session notification =
    case session of
        LoggedIn _ sessionData ->
            LoggedIn (getNavKey session) { sessionData | notifications = notification :: sessionData.notifications }

        s ->
            s


deleteApproveNotificationById : Session -> Int -> Session
deleteApproveNotificationById session idToDelete =
    case session of
        LoggedIn _ sessionData ->
            let
                newNotifications =
                    List.filter (\notification -> not (notification.id == idToDelete)) sessionData.notifications
            in
            LoggedIn (getNavKey session) { sessionData | notifications = newNotifications }

        s ->
            s


deleteApproveNotificationByGameId : Session -> String -> Session
deleteApproveNotificationByGameId session gameIdToDelete =
    case session of
        LoggedIn _ sessionData ->
            let
                hasDiffGameID notification =
                    case notification.acceptData of
                        String gID ->
                            not (gID == gameIdToDelete)

                newNotifications =
                    List.filter hasDiffGameID sessionData.notifications
            in
            LoggedIn (getNavKey session) { sessionData | notifications = newNotifications }

        s ->
            s


simpleNotification : String -> Notification
simpleNotification text =
    Notification text Simple


approveNotification : String -> Metadata -> Notification
approveNotification text acceptData =
    Notification text (Approve acceptData)


computeCurrentId : List NotificationWithId -> Int
computeCurrentId notifications =
    List.map (\( id, _ ) -> id) notifications
        |> List.maximum
        |> Maybe.map (\x -> x + 1)
        |> Maybe.withDefault 0

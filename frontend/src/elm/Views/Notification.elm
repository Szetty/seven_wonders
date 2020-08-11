module Views.Notification exposing (..)

import Common.Domain exposing (AcceptData, Notification, NotificationType(..), NotificationWithId, SavedNotification, savedNotificationToNotificationWithId)
import Common.Session exposing (Session(..), getNavKey, getSavedNotifications)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, hidden)
import Html.Events exposing (onClick)


type Msg
    = OnAcceptFromNotification Int AcceptData
    | RemoveNotification Int


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
      , currentId = 0
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnAcceptFromNotification id _ ->
            ( deleteNotification model id, Cmd.none )

        RemoveNotification id ->
            ( deleteNotification model id, Cmd.none )


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
                |> List.take 3
    in
    div [ class "text-light", hidden isHidden ] notificationsHtml


viewNotification : ( Int, Notification ) -> Html Msg
viewNotification ( id, notification ) =
    let
        buttons =
            case notification.notificationType of
                Simple ->
                    [ button [ onClick (RemoveNotification id), class "btn btn-dark btn-sm ml-2" ] [ text "Ok" ] ]

                Approve accept ->
                    [ button [ onClick (OnAcceptFromNotification id accept), class "btn btn-primary btn-sm ml-2" ] [ text "Accept" ]
                    , button [ onClick (RemoveNotification id), class "btn btn-dark btn-sm ml-2" ] [ text "Decline" ]
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


deleteNotification : Model -> Int -> Model
deleteNotification model idToDelete =
    let
        newNotifications =
            List.filter (\( id, _ ) -> not (id == idToDelete)) model.notifications
    in
    { model | notifications = newNotifications }


addApproveNotification : Session -> SavedNotification -> Session
addApproveNotification session notification =
    case session of
        LoggedIn _ sessionData ->
            LoggedIn (getNavKey session) { sessionData | notifications = notification :: sessionData.notifications }

        s ->
            s


deleteApproveNotification : Session -> Int -> Session
deleteApproveNotification session idToDelete =
    case session of
        LoggedIn _ sessionData ->
            let
                newNotifications =
                    List.filter (\notification -> not (notification.id == idToDelete)) sessionData.notifications
            in
            LoggedIn (getNavKey session) { sessionData | notifications = newNotifications }

        s ->
            s


simpleNotification : String -> Notification
simpleNotification text =
    Notification text Simple


approveNotification : String -> AcceptData -> Notification
approveNotification text acceptData =
    Notification text (Approve acceptData)

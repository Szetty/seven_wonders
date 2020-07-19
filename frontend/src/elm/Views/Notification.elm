module Views.Notification exposing (..)

import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, hidden)
import Html.Events exposing (onClick)


type Msg a
    = OnAcceptFromNotification Int a
    | RemoveNotification Int


type alias Model a =
    { notifications : List ( Int, Notification a )
    , currentId : Int
    }


type alias Notification a =
    { message : String
    , notificationType : NotificationType a
    }


type NotificationType a
    = Simple
    | Approve a


init : ( Model a, Cmd (Msg a) )
init =
    ( { notifications = []
      , currentId = 0
      }
    , Cmd.none
    )


update : Msg a -> Model a -> ( Model a, Cmd (Msg a) )
update msg model =
    case msg of
        OnAcceptFromNotification id _ ->
            ( deleteNotification model id, Cmd.none )

        RemoveNotification id ->
            ( deleteNotification model id, Cmd.none )


view : Model a -> Html (Msg a)
view model =
    div []
        [ viewNotifications model.notifications
        ]


viewNotifications : List ( Int, Notification a ) -> Html (Msg a)
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


viewNotification : ( Int, Notification a ) -> Html (Msg a)
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


addNotification : Model a -> Notification a -> Model a
addNotification model notification =
    { model | notifications = ( model.currentId, notification ) :: model.notifications, currentId = model.currentId + 1 }


deleteNotification : Model a -> Int -> Model a
deleteNotification model idToDelete =
    let
        newNotifications =
            List.filter (\( id, _ ) -> not (id == idToDelete)) model.notifications
    in
    { model | notifications = newNotifications }


simpleNotification : String -> Notification a
simpleNotification text =
    Notification text Simple


approveNotification : String -> a -> Notification a
approveNotification text a =
    Notification text (Approve a)

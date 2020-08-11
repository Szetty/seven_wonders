module Common.Domain exposing (..)

import Json.Decode as Decode exposing (errorToString, field, int, string)
import Json.Encode as Encode


type alias SessionData =
    { userInfo : UserInfo
    , notifications : List SavedNotification
    }


type alias UserInfo =
    { name : String
    , userToken : String
    , gameID : String
    }


type alias SavedNotification =
    { id : Int
    , message : String
    , acceptData : AcceptData
    }


type AcceptData
    = String String


type alias NotificationWithId =
    ( Int, Notification )


type alias Notification =
    { message : String
    , notificationType : NotificationType
    }


type NotificationType
    = Simple
    | Approve AcceptData


userInfoEncoder : UserInfo -> Encode.Value
userInfoEncoder userInfo =
    Encode.object
        [ ( "name", Encode.string userInfo.name )
        , ( "userToken", Encode.string userInfo.userToken )
        , ( "gameID", Encode.string userInfo.gameID )
        ]


notificationsEncoder : List SavedNotification -> Encode.Value
notificationsEncoder notifications =
    Encode.list notificationEncoder notifications


notificationEncoder : SavedNotification -> Encode.Value
notificationEncoder notification =
    Encode.object
        [ ( "id", Encode.int notification.id )
        , ( "message", Encode.string notification.message )
        , ( "acceptData", acceptDataEncoder notification.acceptData )
        ]


acceptDataEncoder : AcceptData -> Encode.Value
acceptDataEncoder acceptData =
    case acceptData of
        String string ->
            Encode.object [ ( "value", Encode.string string ) ]


sessionDataDecoder : Decode.Decoder SessionData
sessionDataDecoder =
    Decode.map2 SessionData
        (field "userInfo" userInfoDecoder)
        (field "notifications" notificationsDecoder)


userInfoDecoder : Decode.Decoder UserInfo
userInfoDecoder =
    Decode.map3 UserInfo
        (field "name" string)
        (field "userToken" string)
        (field "gameID" string)


notificationsDecoder : Decode.Decoder (List SavedNotification)
notificationsDecoder =
    Decode.list notificationDecoder


notificationDecoder : Decode.Decoder SavedNotification
notificationDecoder =
    Decode.map3 SavedNotification
        (field "id" int)
        (field "message" string)
        (field "acceptData" acceptDataDecoder)


acceptDataDecoder : Decode.Decoder AcceptData
acceptDataDecoder =
    Decode.oneOf [ acceptDataStringDecoder ]


acceptDataStringDecoder : Decode.Decoder AcceptData
acceptDataStringDecoder =
    Decode.map String (Decode.field "value" Decode.string)


decodeSessionData : String -> Maybe SessionData
decodeSessionData string =
    case Decode.decodeString sessionDataDecoder string of
        Ok sessionData ->
            Just sessionData

        Err e ->
            Debug.log (errorToString e) Nothing


savedNotificationToNotificationWithId : SavedNotification -> NotificationWithId
savedNotificationToNotificationWithId saveNotification =
    ( saveNotification.id, { message = saveNotification.message, notificationType = Approve saveNotification.acceptData } )

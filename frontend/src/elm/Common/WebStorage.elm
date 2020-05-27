port module Common.WebStorage exposing (..)

import Json.Encode as Encode


type alias UserInfo =
    { name : String
    , userToken : String
    , gameID : String
    }


port storeUserInfo : String -> Cmd msg


port deleteItem : String -> Cmd msg


saveUserInfo : UserInfo -> Cmd msg
saveUserInfo userInfo =
    let
        encoded =
            Encode.encode 0 <|
                Encode.object
                    [ ( "name", Encode.string userInfo.name )
                    , ( "userToken", Encode.string userInfo.userToken )
                    , ( "gameID", Encode.string userInfo.gameID )
                    ]
    in
    storeUserInfo encoded

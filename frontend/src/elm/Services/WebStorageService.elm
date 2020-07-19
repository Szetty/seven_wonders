module Services.WebStorageService exposing (..)

import Json.Encode as Encode
import Networking.WebStorage as WebStorage


type alias UserInfo =
    { name : String
    , userToken : String
    , gameID : String
    }


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
    WebStorage.storeUserInfo encoded


deleteUserInfo : Cmd msg
deleteUserInfo =
    WebStorage.deleteUserInfo ""

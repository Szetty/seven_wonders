module Common.Error exposing (..)


type alias ErrorBody =
    { kind : ErrorType
    , message : String
    }


type ErrorType
    = InvalidName
    | InvalidAccessToken
    | InvalidGameId
    | Unauthorized
    | Unknown
    | InvalidUser

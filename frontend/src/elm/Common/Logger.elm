port module Common.Logger exposing (..)


log : String -> String -> Cmd msg
log prefix toLog =
    doLog ( prefix, toLog )


port doLog : ( String, String ) -> Cmd msg

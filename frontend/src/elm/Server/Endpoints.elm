module Server.Endpoints exposing (..)


login =
    apiPrefix ++ "/login"


game =
    apiPrefix ++ securedPrefix ++ "/game"


apiPrefix =
    "/api"


securedPrefix =
    "/secured"

module Networking.Endpoints exposing (..)


login =
    apiPrefix ++ "/login"


logout =
    apiPrefix ++ securedPrefix ++ "/logout"


game =
    apiPrefix ++ securedPrefix ++ "/game"


apiPrefix =
    "/api"


securedPrefix =
    "/secured"

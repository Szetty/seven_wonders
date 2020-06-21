module Networking.Endpoints exposing (..)


login =
    apiPrefix ++ "/login"


logout =
    apiPrefix ++ "/logout"


game =
    apiPrefix ++ securedPrefix ++ "/game"


apiPrefix =
    "/api"


securedPrefix =
    "/secured"

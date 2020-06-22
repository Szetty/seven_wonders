module Networking.Endpoints exposing (..)


login =
    apiPrefix ++ "/login"


logout =
    apiPrefix ++ securedPrefix ++ "/logout"


checkToken =
    apiPrefix ++ securedPrefix ++ "/checkToken"


game =
    apiPrefix ++ securedPrefix ++ "/game"


apiPrefix =
    "/api"


securedPrefix =
    "/secured"

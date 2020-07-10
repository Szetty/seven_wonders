module Networking.Endpoints exposing (..)


login =
    apiPrefix ++ "/login"


logout =
    securedPrefix ++ "/logout"


checkToken =
    securedPrefix ++ "/checkToken"


game =
    gamePrefix ++ "/:gameID"


gameLobby =
    gamePrefix ++ "/lobby" ++ "/:gameID"


apiPrefix =
    "/api"


securedPrefix =
    apiPrefix ++ "/secured"


gamePrefix =
    securedPrefix ++ "/game"

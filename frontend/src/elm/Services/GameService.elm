module Services.GameService exposing (..)

import Common.Session exposing (Session)
import Networking.Endpoints as Endpoints
import Services.WebSocketService as WebSocketService


initGame : Session -> Cmd msg
initGame session =
    -- TODO temporary needed in order for port to be created
    if 1 == 2 then
        WebSocketService.sendMessage ""

    else
        WebSocketService.init session Endpoints.game


subscriptions : (String -> msg) -> Sub msg
subscriptions =
    WebSocketService.subscriptions

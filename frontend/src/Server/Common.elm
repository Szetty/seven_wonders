module Server.Common exposing (..)

import Http


extractResponse : Result Http.Error a -> Maybe a
extractResponse result =
    case result of
        Ok response ->
            Just response

        Err error ->
            let
                _ =
                    Debug.log "HTTP request failed: " error
            in
            Nothing

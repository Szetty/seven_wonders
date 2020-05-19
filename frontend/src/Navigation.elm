module Navigation exposing (..)

import Browser
import Browser.Navigation as Nav
import Url exposing (Url)


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url


type alias Navigation =
    { key : Nav.Key
    , url : Url
    }


init : Nav.Key -> Url -> Navigation
init =
    Navigation


update : Msg -> Navigation -> ( Navigation, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

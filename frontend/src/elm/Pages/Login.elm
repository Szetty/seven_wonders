module Pages.Login exposing (..)

import Common.Route as Route exposing (Route(..))
import Common.Session exposing (Session, getNavKey, setUserToken)
import Common.WebStorage as WebStorage
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Server.LoginService as LoginService
import Validate exposing (Valid, Validator, fromValid, ifBlank, validate)


type alias Model =
    { session : Session
    , form : Form
    , errors : List String
    }


type alias Form =
    { name : String
    , accessToken : String
    }


type Msg
    = Submit
    | GotName String
    | GotAccessToken String
    | CompletedLogin LoginService.Msg


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , form = initForm
      , errors = []
      }
    , Cmd.none
    )


initForm : Form
initForm =
    { name = ""
    , accessToken = ""
    }


toSession : Model -> Session
toSession { session } =
    session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotName name ->
            let
                form =
                    model.form
            in
            ( { model | form = { form | name = name } }, Cmd.none )

        GotAccessToken accessToken ->
            let
                form =
                    model.form
            in
            ( { model | form = { form | accessToken = accessToken } }, Cmd.none )

        Submit ->
            case validate formValidator model.form of
                Ok validForm ->
                    ( { model | errors = [] }
                    , Cmd.map CompletedLogin (LoginService.login (fromValid validForm))
                    )

                Err errors ->
                    ( { model | errors = errors }
                    , Cmd.none
                    )

        CompletedLogin loginResponse ->
            case LoginService.extractResponse loginResponse of
                Just response ->
                    let
                        _ =
                            ""

                        -- Debug.log "RESPONSE: " response
                        session =
                            setUserToken model.session response

                        cmd =
                            Cmd.batch
                                [ WebStorage.saveUserInfo response
                                , Route.replaceUrl (getNavKey session) Game
                                ]
                    in
                    ( { model | form = initForm, session = session }, cmd )

                Nothing ->
                    ( model, Cmd.none )


view : Model -> List (Html Msg)
view model =
    [ div []
        [ input [ placeholder "Access Token", value model.form.accessToken, onInput GotAccessToken ] []
        , input [ placeholder "Name", value model.form.name, onInput GotName ] []
        , button [ onClick Submit, class "btn btn-primary" ] [ text "Submit" ]
        ]
    , div []
        [ text (String.join "" model.errors)
        ]
    ]


formValidator : Validator String Form
formValidator =
    Validate.all
        [ ifBlank .name "Name can't be blank."
        , ifBlank .accessToken "Access token can't be blank."
        ]

module Pages.Login exposing (..)

import Common.Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import Server.Login as Login
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
    | CompletedLogin Login.Msg


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
            case validate modelValidator model.form of
                Ok validForm ->
                    ( { model | errors = [] }
                    , Cmd.map CompletedLogin (Login.login (fromValid validForm))
                    )

                Err errors ->
                    ( { model | errors = errors }
                    , Cmd.none
                    )

        CompletedLogin loginResponse ->
            case Login.extractResponse loginResponse of
                Just response ->
                    let
                        _ =
                            Debug.log "RESPONSE: " response
                    in
                    ( { model | form = initForm }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


view : Model -> List (Html Msg)
view model =
    [ div []
        [ input [ placeholder "Access Token", value model.form.accessToken, onInput GotAccessToken ] []
        , input [ placeholder "Name", value model.form.name, onInput GotName ] []
        , button [ onClick Submit, class "btn btn-primary" ] [ text "Submit" ]
        ]
    ]


modelValidator : Validator String Form
modelValidator =
    Validate.all
        [ ifBlank .name "Name can't be blank."
        , ifBlank .accessToken "Access token can't be blank."
        ]

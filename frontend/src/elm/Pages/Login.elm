module Pages.Login exposing (..)

import Common.Route as Route exposing (Route(..))
import Common.Session exposing (Session, getNavKey, setUserToken)
import Common.WebStorage as WebStorage
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
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
                Ok response ->
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

                Err errorBody ->
                    let
                        errors =
                            errorBody.errorMessage :: model.errors
                    in
                    ( { model | errors = errors }, Cmd.none )


view : Model -> List (Html Msg)
view model =
    [ div [ class "page-holder bg-cover", style "background-image" "url('%PUBLIC_URL%/7_wonders.jpg')" ]
        [ h1 [ class "p-5" ] [ text "7 WONDERS" ]
        , div [ class "card card-login mt-6 border rounded" ]
            [ div [ class "card-body row justify-content-center" ]
                [ div [ onEnter Submit ]
                    [ div [ class "md-form md-2" ]
                        [ input [ class "form-control", placeholder "Access Token", value model.form.accessToken, onInput GotAccessToken ] []
                        ]
                    , div [ class "md-form mt-2" ]
                        [ input [ class "form-control", placeholder "Name", value model.form.name, onInput GotName ] []
                        ]
                    , div [ class "text-center mt-2" ]
                        [ button [ onClick Submit, class "btn btn-outline-dark" ] [ text "Submit" ] ]
                    ]
                ]
            , p [ class "mt-2", style "color" "red" ] (List.map (\e -> div [] [ text e ]) model.errors)
            ]
        ]
    ]


formValidator : Validator String Form
formValidator =
    Validate.all
        [ ifBlank .accessToken "Access token can't be empty!"
        , ifBlank .name "Name can't be empty!"
        ]


onEnter : msg -> Attribute msg
onEnter msg =
    keyCode
        |> Decode.andThen
            (\key ->
                if key == 13 then
                    Decode.succeed msg

                else
                    Decode.fail "Not enter"
            )
        |> on "keyup"

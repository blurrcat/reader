module Pages.Login exposing (Model, Msg, init, update, subscriptions, view)

import Browser
import Http
import Html exposing (Html, button, div, text, form, input, fieldset)
import Html.Attributes
    exposing
        ( class
        , id
        , disabled
        , required
        , type_
        , placeholder
        , style
        , value
        )
import Html.Events exposing (onInput, onSubmit)
import Api exposing (Cred)
import Session exposing (Session)
import Route


type alias Form =
    { username : String
    , password : String
    , error : Maybe String
    }


type alias Model =
    { form : Form
    , loading : Bool
    , session : Session
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { form =
            { username = ""
            , password = ""
            , error = Nothing
            }
      , loading = False
      , session = session
      }
    , Cmd.none
    )


type Msg
    = Noop
    | InputUsername String
    | InputPassword String
    | SubmitForm
    | CompletedLogin (Result Http.Error Api.Cred)
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        InputUsername username ->
            updateForm (\form -> { form | username = username }) model

        InputPassword password ->
            updateForm (\form -> { form | password = password }) model

        SubmitForm ->
            if isValid model.form then
                ( { model | loading = True }
                , Api.login model.form.username model.form.password CompletedLogin
                )
            else
                -- browsers should have shown some CX in case a required input
                -- is not filled
                ( model, Cmd.none )

        CompletedLogin (Ok cred) ->
            ( { model | loading = False }, Api.storeCred cred )

        CompletedLogin (Err _) ->
            updateForm (\form -> { form | error = Just "Failed to login" })
                { model | loading = False }

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )


updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )


isValid : Form -> Bool
isValid form =
    (String.length form.username) > 0 && (String.length form.password) > 0


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Login"
    , body =
        [ div
            [ id "login-page"
            ]
            [ viewLoginForm model
            ]
        ]
    }


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v msg =
    input
        [ type_ t
        , required True
        , placeholder p
        , value v
        , class "pure-input-1"
        , onInput msg
        ]
        []


viewLoginForm : Model -> Html Msg
viewLoginForm model =
    let
        buttonText =
            if model.loading then
                "Signing in.."
            else
                "Sign in"

        errorView =
            case model.form.error of
                Just err ->
                    div [ class "error red" ] [ text err ]

                Nothing ->
                    text ""
    in
        form
            [ class "pure-form"
            , id "login-form"
            , onSubmit SubmitForm
            ]
            [ fieldset
                [ class "pure-group"
                ]
                [ viewInput "text" "Username" model.form.username InputUsername
                , viewInput "password" "Password" model.form.password InputPassword
                ]
            , errorView
            , button
                [ type_ "submit"
                , class "pure-button pure-input-1 pure-button-primary"
                , disabled model.loading
                ]
                [ text buttonText ]
            ]

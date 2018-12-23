module Pages.Login exposing (Model, Msg, init, update, subscriptions, view)

import Browser
import Html exposing (Html, button, div, text, form, input, fieldset)
import Html.Attributes
    exposing
        ( class
        , id
        , required
        , type_
        , placeholder
        , style
        , value
        )
import Html.Events exposing (onInput)


type alias Model =
    { username : String
    , password : String
    , error : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( { username = ""
      , password = ""
      , error = Nothing
      }
    , Cmd.none
    )


type Msg
    = Noop
    | InputUsername String
    | InputPassword String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        InputUsername username ->
            ( { model | username = username }, Cmd.none )

        InputPassword password ->
            ( { model | password = password }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



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
    form
        [ class "pure-form"
        , id "login-form"
        ]
        [ fieldset
            [ class "pure-group"
            ]
            [ viewInput "text" "Username" model.username InputUsername
            , viewInput "password" "Password" model.password InputPassword
            ]
        , button
            [ type_ "submit"
            , class "pure-button pure-input-1 pure-button-primary"
            ]
            [ text "Sign in" ]
        ]

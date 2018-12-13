module Pages.Login exposing (Model, Msg, init, update, subscriptions, view)

import Browser
import Html exposing (div, text)
import Html.Attributes exposing (class, id)


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


type Msg
    = Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Login"
    , body =
        [ div
            [ id "login-page"
            ]
            [ viewStaticContent []
            , viewLoginForm [] model
            ]
        ]
    }


viewStaticContent attrs =
    div (attrs ++ [])
        [ text "introduction to reader"
        ]


viewLoginForm attrs model =
    div (attrs ++ [])
        [ text "login form"
        ]

module Page.Home exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (class, style)


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    {} ! []


type Msg
    = Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div
        [ class "pure-g" ]
        [ div
            [ class "pure-u-1-4"
            , style [ ( "height", "100%" ) ]
            ]
            [ text "sidebar" ]
        , div
            [ class "pure-u-3-4"
            , style
                [ ( "height", "100%" )
                ]
            ]
            [ text "main" ]
        ]

module Main exposing (main)

import Html
import Page.Home as Home


main : Program Never Home.Model Home.Msg
main =
    Html.program
        { init = Home.init
        , view = Home.view
        , update = Home.update
        , subscriptions = Home.subscriptions
        }

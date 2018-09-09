module Main exposing (main)

import Browser
import Page.Home as Home


main =
    Browser.element
        { init = Home.init
        , view = Home.view
        , update = Home.update
        , subscriptions = Home.subscriptions
        }

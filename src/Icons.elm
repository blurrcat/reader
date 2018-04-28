module Icons exposing (rss)

import FeatherIcons exposing (customIcon, Icon)
import Svg
import Svg.Attributes exposing (..)


rss : Icon
rss =
    [ Svg.path [ d "M4 11a9 9 0 0 1 9 9" ] []
    , Svg.path [ d "M4 4a16 16 0 0 1 16 16" ] []
    , Svg.circle [ cx "5", cy "19", r "1" ] []
    ]
        |> customIcon

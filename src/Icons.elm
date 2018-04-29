module Icons
    exposing
        ( externalLink
        , rss
        , loader
        , Icon
        , customIcon
        , withSize
        , withSizeUnit
        , withClass
        , withStrokeWidth
        , withViewBox
        , toHtml
        , toHtmlStyled
        )

import Html exposing (Html)
import Html.Styled as Styled exposing (fromUnstyled)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)


-- the following are borrowed from elm-feather
-- we only keep the few icons we use to reduce build size


type alias IconAttributes =
    { size : Float
    , sizeUnit : String
    , strokeWidth : Float
    , class : Maybe String
    , viewBox : String
    }


defaultAttributes : String -> IconAttributes
defaultAttributes name =
    { size = 24
    , sizeUnit = ""
    , strokeWidth = 2
    , class = Just <| "feather feather-" ++ name
    , viewBox = "0 0 24 24"
    }


type Icon
    = Icon
        { attrs : IconAttributes
        , src : List (Svg Never)
        }


customIcon : List (Svg Never) -> Icon
customIcon src =
    Icon
        { src = src
        , attrs = IconAttributes 24 "" 2 Nothing "0 0 24 24"
        }


withSize : Float -> Icon -> Icon
withSize size (Icon { attrs, src }) =
    Icon { attrs = { attrs | size = size }, src = src }


withSizeUnit : String -> Icon -> Icon
withSizeUnit sizeUnit (Icon { attrs, src }) =
    Icon { attrs = { attrs | sizeUnit = sizeUnit }, src = src }


withStrokeWidth : Float -> Icon -> Icon
withStrokeWidth strokeWidth (Icon { attrs, src }) =
    Icon { attrs = { attrs | strokeWidth = strokeWidth }, src = src }


withViewBox : String -> Icon -> Icon
withViewBox viewBox (Icon { attrs, src }) =
    Icon { attrs = { attrs | viewBox = viewBox }, src = src }


withClass : String -> Icon -> Icon
withClass class (Icon { attrs, src }) =
    Icon { attrs = { attrs | class = Just class }, src = src }


toHtml : List (Svg.Attribute msg) -> Icon -> Html msg
toHtml attributes (Icon { src, attrs }) =
    let
        strSize =
            attrs.size |> toString

        baseAttributes =
            [ fill "none"
            , height <| strSize ++ attrs.sizeUnit
            , width <| strSize ++ attrs.sizeUnit
            , stroke "currentColor"
            , strokeLinecap "round"
            , strokeLinejoin "round"
            , strokeWidth <| toString attrs.strokeWidth
            , viewBox attrs.viewBox
            ]

        combinedAttributes =
            (case attrs.class of
                Just c ->
                    (class c) :: baseAttributes

                Nothing ->
                    baseAttributes
            )
                ++ attributes
    in
        src
            |> List.map (Svg.map never)
            |> svg combinedAttributes


toHtmlStyled : List (Svg.Attribute msg) -> Icon -> Styled.Html msg
toHtmlStyled attributes icon =
    icon
        |> toHtml attributes
        |> fromUnstyled


externalLink : Icon
externalLink =
    customIcon
        [ Svg.path [ d "M18 13v6a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h6" ] []
        , Svg.polyline [ points "15 3 21 3 21 9" ] []
        , Svg.line [ x1 "10", y1 "14", x2 "21", y2 "3" ] []
        ]


rss : Icon
rss =
    customIcon
        [ Svg.path [ d "M4 11a9 9 0 0 1 9 9" ] []
        , Svg.path [ d "M4 4a16 16 0 0 1 16 16" ] []
        , Svg.circle [ cx "5", cy "19", r "1" ] []
        ]


loader : Icon
loader =
    customIcon
        [ Svg.line [ x1 "12", y1 "2", x2 "12", y2 "6" ] []
        , Svg.line [ x1 "12", y1 "18", x2 "12", y2 "22" ] []
        , Svg.line [ x1 "4.93", y1 "4.93", x2 "7.76", y2 "7.76" ] []
        , Svg.line [ x1 "16.24", y1 "16.24", x2 "19.07", y2 "19.07" ] []
        , Svg.line [ x1 "2", y1 "12", x2 "6", y2 "12" ] []
        , Svg.line [ x1 "18", y1 "12", x2 "22", y2 "12" ] []
        , Svg.line [ x1 "4.93", y1 "19.07", x2 "7.76", y2 "16.24" ] []
        , Svg.line [ x1 "16.24", y1 "7.76", x2 "19.07", y2 "4.93" ] []
        ]

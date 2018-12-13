module Route
    exposing
        ( Route(..)
        , fromUrl
        )

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string, top)


type Route
    = Home
    | Login


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home top
        , Parser.map Login (s "login")
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url

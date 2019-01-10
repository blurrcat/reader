module Api.Endpoint exposing (request, Endpoint, feeds, feedItems, tokens, refreshTokens)

import Http
import Url.Builder exposing (QueryParameter)


{-| Http.request, except it takes an Endpoint instead of a Url.
-}
request :
    { url : Endpoint
    , method : String
    , headers : List Http.Header
    , body : Http.Body
    , expect : Http.Expect a
    , timeout : Maybe Float
    , tracker : Maybe String
    }
    -> Cmd a
request config =
    Http.request
        { body = config.body
        , expect = config.expect
        , headers = config.headers
        , method = config.method
        , timeout = config.timeout
        , url = unwrap config.url
        , tracker = config.tracker
        }



-- TYPES


{-| Get a URL to the Conduit API.
This is not publicly exposed, because we want to make sure the only way to get one of these URLs is from this module.
-}
type Endpoint
    = Endpoint String


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str


url : List String -> List QueryParameter -> Endpoint
url paths queryParams =
    -- NOTE: Url.Builder takes care of percent-encoding special URL characters.
    -- See https://package.elm-lang.org/packages/elm/url/latest/Url#percentEncode
    Url.Builder.crossOrigin "https://air-api.blurrcat.net"
        paths
        queryParams
        |> Endpoint



-- ENDPOINTS


feeds : List QueryParameter -> Endpoint
feeds params =
    url [ "feeds/" ] params


feedItems : List QueryParameter -> Endpoint
feedItems params =
    url [ "feed-items/" ] params


tokens : Endpoint
tokens =
    url [ "tokens/" ] []


refreshTokens : Endpoint
refreshTokens =
    url [ "tokens", "refresh/" ] []

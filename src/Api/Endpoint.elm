module Api.Endpoint exposing (request, Endpoint, feeds, feedItems)

import Http
import Url.Builder exposing (QueryParameter)


authHeader =
    Http.header "Authorization" "Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ0b2tlbl90eXBlIjoic2xpZGluZyIsImV4cCI6MTU0NjIxMjk1OCwianRpIjoiOWY2NTU3ZTU5YzNhNGI1MWE3Y2Q4NzQ1ZTg4ODE1NWIiLCJyZWZyZXNoX2V4cCI6MTU0ODIwMDE1OCwidXNlcl9pZCI6MX0._j9KJlVFJ7npmbMR3PpEDFLmzGQ613xbIvyzxbKy7IY"


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
        , headers = authHeader :: config.headers
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


login : Endpoint
login =
    url [ "tokens/" ] []


refreshToken : Endpoint
refreshToken =
    url [ "tokens", "refresh/" ] []

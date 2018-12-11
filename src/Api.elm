module Api exposing (ResultMsg, get)

import Api.Endpoint as Endpoint exposing (Endpoint)
import Http
import Json.Decode as Decode exposing (Decoder)


type alias ResultMsg a msg =
    Result Http.Error a -> msg


get : Endpoint -> ResultMsg a msg -> Decoder a -> Cmd msg
get url msg decoder =
    Endpoint.request
        { method = "GET"
        , url = url
        , headers = []
        , body = Http.emptyBody
        , expect = Http.expectJson msg decoder
        , timeout = Nothing
        , tracker = Nothing
        }

module Api exposing (url, get, list, ListResponse, listDecoder)

import Http
import Json.Decode as Decode exposing (Decoder, int, string, nullable)
import Json.Decode.Pipeline exposing (decode, required, requiredAt)
import HttpBuilder
    exposing
        ( withHeader
        , withQueryParams
        , withExpectJson
        , toRequest
        )


type alias ListResponse a =
    { count : Int
    , next : Maybe Int
    , previous : Maybe Int
    , results : List a
    }


listDecoder : Decoder a -> Decoder (ListResponse a)
listDecoder itemDecoder =
    decode ListResponse
        |> required "count" int
        |> required "next" (nullable int)
        |> required "previous" (nullable int)
        |> required "results" (Decode.list itemDecoder)


url : String -> String
url str =
    "https://air-api.blurrcat.net" ++ str



-- "http://localhost:8000" ++ str


get : List ( String, String ) -> Decoder a -> String -> Http.Request a
get params decoder endpoint =
    url endpoint
        |> HttpBuilder.get
        |> withHeader "Accept" "application/json"
        |> withQueryParams params
        |> withExpectJson decoder
        |> toRequest


list : List ( String, String ) -> Decoder a -> String -> Http.Request (ListResponse a)
list params decoder endpoint =
    get params (listDecoder decoder) endpoint

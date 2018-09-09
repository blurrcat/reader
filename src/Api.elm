module Api exposing (ListResponse, get, list, listDecoder, url)

import Http
import HttpBuilder
    exposing
        ( toRequest
        , withExpectJson
        , withHeader
        , withQueryParams
        )
import Json.Decode as Decode exposing (Decoder, int, nullable, string)
import Json.Decode.Pipeline exposing (required, requiredAt)


type alias ListResponse a =
    { count : Int
    , next : Maybe Int
    , previous : Maybe Int
    , results : List a
    }


listDecoder : Decoder a -> Decoder (ListResponse a)
listDecoder itemDecoder =
    Decode.succeed ListResponse
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

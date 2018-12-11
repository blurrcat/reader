module PaginatedList exposing (PaginatedList, params, decoder, currentPage)

import Json.Decode as Decode exposing (Decoder, int, nullable, string)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Url.Builder exposing (QueryParameter)


type alias PaginatedList a =
    { count : Int
    , next : Maybe Int
    , previous : Maybe Int
    , results : List a
    }


currentPage : PaginatedList a -> Int
currentPage list =
    case list.next of
        Just next ->
            next - 1

        Nothing ->
            case list.previous of
                Just prev ->
                    prev + 1

                Nothing ->
                    0



-- serialization


decoder : Decoder a -> Decoder (PaginatedList a)
decoder itemDecoder =
    Decode.succeed PaginatedList
        |> required "count" int
        |> required "next" (nullable int)
        |> required "previous" (nullable int)
        |> required "results" (Decode.list itemDecoder)


params : { page : Int, perPage : Int } -> List QueryParameter
params { page, perPage } =
    [ Url.Builder.string "page" (String.fromInt page)
    , Url.Builder.string "per_page" (String.fromInt perPage)
    ]

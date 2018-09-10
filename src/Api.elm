module Api
    exposing
        ( ListResponse
        , FeedsResponse
        , FeedItemsResponse
        , currentPage
        , listFeedsRequest
        , listFeedItemsRequest
        )

import Http
import HttpBuilder
    exposing
        ( toRequest
        , withExpectJson
        , withHeader
        , withQueryParams
        )
import RemoteData exposing (WebData)
import Json.Decode as Decode exposing (Decoder, int, nullable, string)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Data.Feed as Feed
import Data.Feed.Item as FeedItem


type alias ListResponse a =
    { count : Int
    , next : Maybe Int
    , previous : Maybe Int
    , results : List a
    }


type alias WebDataListResponse a =
    WebData (ListResponse a)


currentPage : ListResponse a -> Int
currentPage resp =
    case resp.next of
        Just next ->
            next - 1

        Nothing ->
            case resp.previous of
                Just prev ->
                    prev + 1

                Nothing ->
                    0


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


httpGet : List ( String, String ) -> Decoder a -> String -> Http.Request a
httpGet params decoder endpoint =
    url endpoint
        |> HttpBuilder.get
        |> withHeader "Accept" "application/json"
        |> withQueryParams params
        |> withExpectJson decoder
        |> toRequest


list : List ( String, String ) -> Decoder a -> (WebDataListResponse a -> b) -> String -> Cmd b
list params decoder msg endpoint =
    httpGet params (listDecoder decoder) endpoint
        |> RemoteData.sendRequest
        |> Cmd.map msg


type alias FeedsResponse =
    WebData (ListResponse Feed.Feed)


type alias FeedItemsResponse =
    WebData (ListResponse FeedItem.FeedItem)


listFeedsRequest : Int -> (FeedsResponse -> a) -> Cmd a
listFeedsRequest page msg =
    "/feeds/"
        |> list
            [ ( "page", String.fromInt page ) ]
            Feed.decoder
            msg


listFeedItemsRequest : Int -> Maybe Feed.FeedId -> (FeedItemsResponse -> a) -> Cmd a
listFeedItemsRequest page feedId msg =
    "/feed-items/"
        |> list
            [ ( "page", String.fromInt page )
            , ( "feed_id"
              , feedId
                    |> Maybe.map Feed.idToString
                    |> Maybe.withDefault ""
              )
            ]
            FeedItem.decoder
            msg

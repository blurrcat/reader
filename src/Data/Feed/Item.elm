module Data.Feed.Item exposing (FeedItem, FeedItemId, Paginated, decoder, feedItemIdDecoder, idToString, list)

import Api
import Api.Endpoint as Endpoint
import Url.Builder exposing (QueryParameter)
import PaginatedList exposing (PaginatedList)
import Data.Feed as Feed
import String
import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (optional, required)


type alias FeedItem =
    { id : FeedItemId
    , title : String
    , description : String
    , link : String
    , updated_at : String
    }



-- SERIALIZATION --


decoder : Decoder FeedItem
decoder =
    Decode.succeed FeedItem
        |> required "id" feedItemIdDecoder
        |> required "title" string
        |> optional "description" string ""
        |> required "link" string
        |> required "updated_at" string



-- IDENTIFIERS --


type FeedItemId
    = FeedItemId Int


idToString : FeedItemId -> String
idToString (FeedItemId id) =
    String.fromInt id


feedItemIdDecoder : Decoder FeedItemId
feedItemIdDecoder =
    Decode.map FeedItemId int



-- API --


type alias Paginated =
    PaginatedList FeedItem


list :
    List QueryParameter
    -> Maybe Feed.FeedId
    -> Api.ResultMsg Paginated msg
    -> Cmd msg
list params maybeFeedId msg =
    let
        feedIdQuery =
            maybeFeedId
                |> Maybe.map Feed.idToString
                |> Maybe.withDefault ""
                |> Url.Builder.string "feed_id"
    in
        Api.get
            (Endpoint.feedItems (feedIdQuery :: params))
            msg
            (PaginatedList.decoder decoder)

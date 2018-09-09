module Data.Feed.Item exposing (FeedItem, FeedItemId, decoder, feedItemIdDecoder, idToString)

import Data.Feed exposing (FeedId, feedIdDecoder)
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

module Data.Feed.Item exposing (FeedItem, FeedItemId, decoder, feedItemIdDecoder, idToString)

import Date exposing (Date)
import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Extra exposing (date)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Data.Feed exposing (FeedId, feedIdDecoder)


type alias FeedItem =
    { id : FeedItemId
    , title : String
    , description : String
    , link : String
    , updated_at : Date
    }



-- SERIALIZATION --


decoder : Decoder FeedItem
decoder =
    decode FeedItem
        |> required "id" feedItemIdDecoder
        |> required "title" string
        |> optional "description" string ""
        |> required "link" string
        |> required "updated_at" date



-- IDENTIFIERS --


type FeedItemId
    = FeedItemId Int


idToString : FeedItemId -> String
idToString (FeedItemId id) =
    toString id


feedItemIdDecoder : Decoder FeedItemId
feedItemIdDecoder =
    Decode.map FeedItemId int

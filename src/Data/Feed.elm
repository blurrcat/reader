module Data.Feed exposing (Feed, FeedId, decoder, feedIdDecoder, idFromInt, idToString)

import String
import Time
import Json.Decode as Decode exposing (Decoder, int, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)


type alias Feed =
    { id : FeedId
    , title : String
    , description : String
    , link : Maybe String
    , feed_link : String
    , updated_at : String
    }



-- SERIALIZATION --


decoder : Decoder Feed
decoder =
    Decode.succeed Feed
        |> required "id" feedIdDecoder
        |> required "title" string
        |> optional "description" string ""
        |> required "link" (nullable string)
        |> required "feed_link" string
        |> required "updated_at" string



-- IDENTIFIERS --


type FeedId
    = FeedId Int


idFromInt : Int -> FeedId
idFromInt id =
    FeedId id


idToString : FeedId -> String
idToString (FeedId id) =
    String.fromInt id


feedIdDecoder : Decoder FeedId
feedIdDecoder =
    Decode.map FeedId int

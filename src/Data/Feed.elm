module Data.Feed exposing (Feed, FeedId, feedIdDecoder, decoder, idToString, idFromInt)

import Date exposing (Date)
import Json.Decode as Decode exposing (Decoder, int, string, nullable)
import Json.Decode.Extra exposing (date)
import Json.Decode.Pipeline exposing (decode, required, optional)


type alias Feed =
    { id : FeedId
    , title : String
    , description : String
    , link : Maybe String
    , updated_at : Date
    }



-- SERIALIZATION --


decoder : Decoder Feed
decoder =
    decode Feed
        |> required "id" feedIdDecoder
        |> required "title" string
        |> optional "description" string ""
        |> required "link" (nullable string)
        |> required "updated_at" date



-- IDENTIFIERS --


type FeedId
    = FeedId Int


idFromInt : Int -> FeedId
idFromInt id =
    FeedId id


idToString : FeedId -> String
idToString (FeedId id) =
    toString id


feedIdDecoder : Decoder FeedId
feedIdDecoder =
    Decode.map FeedId int

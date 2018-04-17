module Data.FeedTest exposing (suite)

import Test exposing (..)
import Expect exposing (Expectation)
import Date
import Json.Decode exposing (decodeString)
import Data.Feed as Feed


suite : Test
suite =
    describe "Data.Feed"
        [ test "Feed decoder" <|
            \_ ->
                case Date.fromString "2012-09-25T00:00:00Z" of
                    Ok updated_at ->
                        """{"id":28,"title":"Idle Words","description":"Brevity is for the weak","link":null,"updated_at":"2012-09-25T00:00:00Z","feed_link":"http://idlewords.com/index.xml"}"""
                            |> decodeString Feed.decoder
                            |> Expect.equal
                                (Ok
                                    { id = Feed.idFromInt 28
                                    , title = "Idle Words"
                                    , description = "Brevity is for the weak"
                                    , link = Nothing
                                    , updated_at = updated_at
                                    }
                                )

                    Err e ->
                        "cannot parse the date: "
                            ++ e
                            |> Expect.fail
        ]

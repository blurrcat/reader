module ApiTest exposing (suite)

import Test exposing (..)
import Expect exposing (Expectation)
import Json.Decode exposing (decodeString)
import Data.Feed as Feed
import Api exposing (..)
import Debug


suite : Test
suite =
    describe "Api"
        [ test "ListResponse decoder" <|
            \_ ->
                let
                    raw =
                        """{"count":31,"next":null,"previous":1,"results":[{"id":16,"title":"Ferd.ca","description":"My own blog about programming and whatnot.","link":null,"updated_at":"2018-04-05T16:55:00Z","feed_link":"https://ferd.ca/feed.rss"},{"id":9,"title":"Technology – Metamarkets","description":"","link":null,"updated_at":"2018-03-29T19:06:59Z","feed_link":"https://metamarkets.com/category/technology/feed/"},{"id":24,"title":"GitHub Engineering","description":"The Blog of the GitHub Engineering Team","link":null,"updated_at":"2018-03-16T22:30:41Z","feed_link":"https://githubengineering.com/atom.xml"},{"id":31,"title":"Base Lab","description":"Tech team behind Base","link":null,"updated_at":"2018-02-19T10:05:53Z","feed_link":"https://lab.getbase.com/feed/"},{"id":20,"title":"Hacking Distributed","description":null,"link":null,"updated_at":"2018-01-18T09:30:00Z","feed_link":"http://hackingdistributed.com/hackingdistributed.atom"},{"id":5,"title":"Apple Machine Learning Journal","description":"Join our team.","link":null,"updated_at":"2017-12-04T23:31:06Z","feed_link":"https://machinelearning.apple.com/feed.xml"},{"id":8,"title":"Nick Craver","description":"Software Imagineering","link":null,"updated_at":"2017-11-01T23:00:17Z","feed_link":"https://nickcraver.com/blog/feed.xml"},{"id":30,"title":"Evan Miller’s News","description":"Evan Miller’s News","link":null,"updated_at":"2017-08-13T18:05:00Z","feed_link":"http://www.evanmiller.org/news.xml"},{"id":12,"title":"Devlog on Glider Labs, Open Source Innovation Lab","description":"Recent content in Devlog on Glider Labs, Open Source Innovation Lab","link":null,"updated_at":"2016-12-21T23:00:00Z","feed_link":"https://gliderlabs.com/devlog/index.xml"},{"id":25,"title":"colah's blog","description":"","link":null,"updated_at":"2015-09-03T00:00:00Z","feed_link":"http://colah.github.io/rss.xml"},{"id":28,"title":"Idle Words","description":"Brevity is for the weak","link":null,"updated_at":"2012-09-25T00:00:00Z","feed_link":"http://idlewords.com/index.xml"}]}"""
                in
                    case decodeString (Api.listDecoder Feed.decoder) raw of
                        Ok data ->
                            if data.count == 31 then
                                Expect.pass
                            else
                                Expect.fail "count is not 31!"

                        Err err ->
                            Expect.fail err
        ]

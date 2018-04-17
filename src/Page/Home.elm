module Page.Home exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import RemoteData exposing (WebData)
import Json.Decode as Decode
import Data.Feed as Feed
import Data.Feed.Item as FeedItem
import Http
import HttpBuilder
import Api


type alias Feeds =
    Api.ListResponse Feed.Feed


type alias FeedItems =
    Api.ListResponse FeedItem.FeedItem


type alias Model =
    { feeds : WebData Feeds
    , feedItems : WebData FeedItems
    , selectedFeedId : Maybe Feed.FeedId
    }


getFeeds page =
    "/feeds/"
        |> Api.list
            [ ( "page", toString page ) ]
            Feed.decoder
        |> RemoteData.sendRequest
        |> Cmd.map FeedsResponse


getFeedItems page feedId =
    ("/feeds/" ++ (Feed.idToString feedId) ++ "/items")
        |> Api.list
            [ ( "page", toString page ) ]
            FeedItem.decoder
        |> RemoteData.sendRequest
        |> Cmd.map FeedItemsResponse


init : ( Model, Cmd Msg )
init =
    { feeds = RemoteData.Loading
    , feedItems = RemoteData.NotAsked
    , selectedFeedId = Nothing
    }
        ! [ getFeeds 1 ]


type Msg
    = Noop
    | FeedsResponse (WebData Feeds)
    | FeedItemsResponse (WebData FeedItems)
    | SelectFeed Feed.FeedId


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            model ! []

        FeedsResponse resp ->
            { model | feeds = resp } ! []

        FeedItemsResponse resp ->
            { model | feedItems = resp } ! []

        SelectFeed feedId ->
            { model | selectedFeedId = Just feedId }
                ! [ getFeedItems 1 feedId ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


feedView : Msg -> Maybe Feed.FeedId -> Feed.Feed -> Html Msg
feedView onClickMsg selectedFeedId feed =
    let
        selectedStyle =
            case selectedFeedId of
                Just fid ->
                    if feed.id == fid then
                        ( "font-weight", "bold" )
                    else
                        ( "font-weight", "normal" )

                Nothing ->
                    ( "font-weight", "normal" )

        styles =
            selectedStyle
                :: [ ( "borderBottom", "1px solid #ddd" )
                   , ( "padding", "0.5em" )
                   ]
    in
        div
            [ style styles, onClick onClickMsg ]
            [ text feed.title
            ]


feedsView : Maybe Feed.FeedId -> WebData Feeds -> Html Msg
feedsView selectedFeedId feeds =
    let
        content =
            case feeds of
                RemoteData.NotAsked ->
                    text "Initializing.."

                RemoteData.Loading ->
                    text "Loading.."

                RemoteData.Failure err ->
                    text ("Error: " ++ toString err)

                RemoteData.Success resp ->
                    resp.results
                        |> List.map (\f -> feedView (SelectFeed f.id) selectedFeedId f)
                        |> div []
    in
        div
            [ style
                [ ( "borderRight", "1px solid #555" )
                ]
            ]
            [ content ]


feedItemView item =
    div
        [ style
            [ ( "borderBottom", "1px solid #ddd" )
            , ( "padding", "0.5em" )
            ]
        ]
        [ text item.title
        ]


feedItemsView : WebData FeedItems -> Html Msg
feedItemsView items =
    let
        content =
            case items of
                RemoteData.NotAsked ->
                    text ""

                RemoteData.Loading ->
                    text "Loading.."

                RemoteData.Failure err ->
                    text ("Error: " ++ toString err)

                RemoteData.Success resp ->
                    resp.results
                        |> List.map feedItemView
                        |> div []
    in
        div
            []
            [ content ]


view : Model -> Html Msg
view model =
    div
        [ class "pure-g" ]
        [ div
            [ class "pure-u-1-4"
            , style
                [ ( "height", "100%" )
                ]
            ]
            [ feedsView model.selectedFeedId model.feeds ]
        , div
            [ class "pure-u-3-4"
            , style
                [ ( "height", "100%" )
                ]
            ]
            [ feedItemsView model.feedItems ]
        ]

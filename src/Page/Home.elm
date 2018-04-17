module Page.Home exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (class, style, href)
import Html.Events exposing (onClick)
import RemoteData exposing (WebData)
import Json.Decode as Decode
import Data.Feed as Feed
import Data.Feed.Item as FeedItem
import Http
import HttpBuilder
import Api
import Markdown
import Date.Format as DF


type alias Feeds =
    Api.ListResponse Feed.Feed


type alias FeedItems =
    Api.ListResponse FeedItem.FeedItem


type alias Model =
    { feeds : WebData Feeds
    , feedItems : WebData FeedItems
    , selectedFeedId : Maybe Feed.FeedId
    , selectedFeedItemId : Maybe FeedItem.FeedItemId
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
    , selectedFeedItemId = Nothing
    }
        ! [ getFeeds 1 ]


type Msg
    = Noop
    | FeedsResponse (WebData Feeds)
    | FeedItemsResponse (WebData FeedItems)
    | SelectFeed Feed.FeedId
    | SelectFeedItem FeedItem.FeedItemId


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
            { model
                | selectedFeedId = Just feedId
                , feedItems = RemoteData.Loading
            }
                ! [ getFeedItems 1 feedId ]

        SelectFeedItem feedItemId ->
            let
                newSelectedId =
                    case model.selectedFeedItemId of
                        Nothing ->
                            Just feedItemId

                        Just currentId ->
                            if currentId == feedItemId then
                                Nothing
                            else
                                Just feedItemId
            in
                { model | selectedFeedItemId = newSelectedId } ! []


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
                   , ( "cursor", "pointer" )
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


feedItemView onClickMsg selectedId item =
    let
        isSelected =
            case selectedId of
                Nothing ->
                    False

                Just selected ->
                    selected == item.id

        titleWeight =
            if isSelected then
                "bold"
            else
                "normal"

        titleView =
            [ div
                [ onClick onClickMsg ]
                [ span
                    [ style
                        [ ( "font-weight", titleWeight )
                        , ( "cursor", "pointer" )
                        ]
                    ]
                    [ text item.title ]
                , span
                    [ style
                        [ ( "font-size", "80%" )
                        , ( "color", "#aaa" )
                        , ( "margin-left", "0.5em" )
                        ]
                    ]
                    [ text (DF.format "%Y-%m-%d %H:%M:%S" item.updated_at) ]
                ]
            ]

        detailView =
            if isSelected then
                [ div
                    [ style
                        [ ( "padding", "1em" )
                        , ( "font-size", "90%" )
                        ]
                    ]
                    [ a [ href item.link ] [ text item.link ]
                    , Markdown.toHtml [] item.description
                    ]
                ]
            else
                []
    in
        div
            [ style
                [ ( "borderBottom", "1px solid #ddd" )
                , ( "padding", "0.5em" )
                ]
            ]
            (titleView ++ detailView)


feedItemsView : Maybe FeedItem.FeedItemId -> WebData FeedItems -> Html Msg
feedItemsView selectedId items =
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
                        |> List.map (\item -> feedItemView (SelectFeedItem item.id) selectedId item)
                        |> div []
    in
        div
            [ style [ ( "padding", "1em" ) ] ]
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
            [ feedItemsView model.selectedFeedItemId model.feedItems ]
        ]

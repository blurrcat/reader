module Page.Home exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (..)
import Html.Attributes
    exposing
        ( style
        , class
        , classList
        , href
        , rel
        , title
        , id
        , target
        )
import Html.Events exposing (onClick)
import RemoteData exposing (WebData)
import Json.Decode as Decode
import Data.Feed as Feed
import Data.Feed.Item as FeedItem
import Http
import HttpBuilder
import Api
import Markdown
import Date
import Date.Format as DF
import Icons


type alias Feeds =
    Api.ListResponse Feed.Feed


type alias FeedItems =
    Api.ListResponse FeedItem.FeedItem


type alias Model =
    { feeds : WebData Feeds
    , feedItems : WebData FeedItems
    , selectedFeed : Maybe Feed.Feed
    , selectedFeedItemId : Maybe FeedItem.FeedItemId
    , menuActive : Bool
    }


getFeeds page =
    "/feeds/"
        |> Api.list
            [ ( "page", toString page ) ]
            Feed.decoder
        |> RemoteData.sendRequest
        |> Cmd.map FeedsResponse


getFeedItems page feedId =
    "/feed-items/"
        |> Api.list
            [ ( "page", toString page )
            , ( "feed_id"
              , feedId
                    |> Maybe.map Feed.idToString
                    |> Maybe.withDefault ""
              )
            ]
            FeedItem.decoder
        |> RemoteData.sendRequest
        |> Cmd.map FeedItemsResponse


init : ( Model, Cmd Msg )
init =
    { feeds = RemoteData.Loading
    , feedItems = RemoteData.Loading
    , selectedFeed = Nothing
    , selectedFeedItemId = Nothing
    , menuActive = False
    }
        ! [ getFeeds 1
          , getFeedItems 1 Nothing
          ]


type Msg
    = Noop
    | FeedsResponse (WebData Feeds)
    | FeedItemsResponse (WebData FeedItems)
    | SelectFeed Feed.FeedId
    | SelectFeedItem FeedItem.FeedItemId
    | ToggleMenuActive


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            model ! []

        ToggleMenuActive ->
            { model | menuActive = (not model.menuActive) } ! []

        FeedsResponse resp ->
            { model | feeds = resp } ! []

        FeedItemsResponse resp ->
            { model | feedItems = resp } ! []

        SelectFeed feedId ->
            let
                selectedFeed =
                    model.feeds
                        |> RemoteData.toMaybe
                        |> Maybe.andThen
                            (\resp ->
                                resp.results
                                    |> List.filter (\f -> f.id == feedId)
                                    |> List.head
                            )
            in
                { model
                    | selectedFeed = selectedFeed
                    , menuActive = False
                    , feedItems = RemoteData.Loading
                }
                    ! [ getFeedItems 1 (Just feedId) ]

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
    li
        [ class "pure-menu-item"
        , classList
            [ ( "pure-menu-selected"
              , selectedFeedId
                    |> Maybe.map ((==) feed.id)
                    |> (==) (Just True)
              )
            ]
        , onClick onClickMsg
        ]
        [ a
            [ href "#"
            , title feed.title
            , class "pure-menu-link"
            , style
                [ ( "overflow", "hidden" )
                , ( "text-overflow", "ellipsis" )
                ]
            ]
            [ text feed.title ]
        ]


feedsView : Maybe Feed.FeedId -> WebData Feeds -> Html Msg
feedsView selectedFeedId feeds =
    let
        content =
            case feeds of
                RemoteData.NotAsked ->
                    text "Initializing.."

                RemoteData.Loading ->
                    loadingView

                RemoteData.Failure err ->
                    text ("Error: " ++ toString err)

                RemoteData.Success resp ->
                    resp.results
                        |> List.map (\f -> feedView (SelectFeed f.id) selectedFeedId f)
                        |> ul [ class "pure-menu-list" ]
    in
        div
            [ class "pure-menu" ]
            [ content ]


loadingView : Html msg
loadingView =
    div
        [ class "animated rotateIn loading"
        ]
        [ Icons.loader
            |> Icons.toHtml []
        ]


linkIconView : Icons.Icon -> Maybe String -> Html msg
linkIconView icon url =
    a
        [ href (url |> Maybe.withDefault "#")
        , target "_blank"
        , rel "noopener noreferrer"
        , class "link-icon"
        , classList [ ( "hidden", url == Nothing ) ]
        ]
        [ icon
            |> Icons.withSize 1
            |> Icons.withSizeUnit "em"
            |> Icons.toHtml []
        ]


datetimeView : Date.Date -> Html msg
datetimeView datetime =
    span
        [ class "datetime"
        ]
        [ text (DF.format "%Y-%m-%d %H:%M:%S" datetime)
        ]


feedItemView : Msg -> Maybe FeedItem.FeedItemId -> FeedItem.FeedItem -> Html Msg
feedItemView onClickMsg selectedId item =
    let
        isSelected =
            case selectedId of
                Nothing ->
                    False

                Just selected ->
                    selected == item.id

        titleView =
            div
                [ onClick onClickMsg
                , class "title"
                , classList [ ( "selected", isSelected ) ]
                ]
                [ text item.title ]

        detailView =
            if isSelected then
                [ div
                    [ class "body"
                    ]
                    [ div []
                        [ datetimeView item.updated_at
                        , linkIconView Icons.externalLink (Just item.link)
                        ]
                    , item.description
                        |> Markdown.toHtml []
                    ]
                ]
            else
                []
    in
        div
            [ class "feed-item"
            ]
            (titleView :: detailView)


feedItemsView : Maybe Feed.Feed -> Maybe FeedItem.FeedItemId -> WebData FeedItems -> Html Msg
feedItemsView selectedFeed selectedId items =
    let
        titleContent =
            case selectedFeed of
                Nothing ->
                    [ h3 [] [ text "Latest" ]
                    ]

                Just feed ->
                    [ h3 [] [ text feed.title ]
                    , div
                        []
                        [ div []
                            [ datetimeView feed.updated_at
                            , linkIconView Icons.rss (Just feed.feed_link)
                            , linkIconView Icons.externalLink feed.link
                            ]
                        ]
                    , p
                        [ class "desc"
                        ]
                        [ text feed.description ]
                    ]

        titleDiv =
            div
                [ class "feed"
                ]
                titleContent

        content =
            case items of
                RemoteData.NotAsked ->
                    text ""

                RemoteData.Loading ->
                    loadingView

                RemoteData.Failure err ->
                    text ("Error: " ++ toString err)

                RemoteData.Success resp ->
                    div
                        [ class "feed-items" ]
                        [ titleDiv
                        , div [ class "animated fadeIn" ]
                            (resp.results
                                |> List.map
                                    (\item -> feedItemView (SelectFeedItem item.id) selectedId item)
                            )
                        ]
    in
        content


view : Model -> Html Msg
view model =
    let
        menuButtonIcon =
            if model.menuActive then
                Icons.x
            else
                Icons.menu
    in
        div
            -- layout
            [ id "home"
            ]
            [ a
                -- menu button
                [ id "menu-btn"
                , classList [ ( "active", model.menuActive ) ]
                , onClick ToggleMenuActive
                ]
                [ menuButtonIcon
                    |> Icons.toHtml []
                ]
            , div
                [ classList [ ( "active", model.menuActive ) ]
                , id "menu"
                ]
                [ model.feeds
                    |> feedsView (model.selectedFeed |> Maybe.map .id)
                ]
            , div
                -- main
                [ id "main" ]
                [ div
                    [ class "overlay"
                    , classList [ ( "active", model.menuActive ) ]
                    ]
                    []
                , div
                    [ id "header"
                    ]
                    []
                , div
                    [ id "content"
                    ]
                    [ model.feedItems
                        |> feedItemsView model.selectedFeed model.selectedFeedItemId
                    ]
                ]
            ]

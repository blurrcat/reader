module Page.Home exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (class, classList, style, href, target, rel)
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
import FeatherIcons as FIcons
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
                    loadingView

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


loadingView : Html msg
loadingView =
    div
        [ style
            [ ( "width", "100%" )
            , ( "text-align", "center" )
            ]
        , class "animated rotateIn"
        ]
        [ FIcons.loader
            |> FIcons.toHtml []
        ]


linkIconView : FIcons.Icon -> Maybe String -> Html msg
linkIconView icon url =
    a
        [ href (url |> Maybe.withDefault "#")
        , target "_blank"
        , rel "noopener noreferrer"
        , style
            [ ( "color", "black" )
            , ( "padding-right", "0.5em" )
            ]
        , classList [ ( "hidden", url == Nothing ) ]
        ]
        [ icon
            |> FIcons.withSize 1
            |> FIcons.withSizeUnit "em"
            |> FIcons.toHtml []
        ]


datetimeView : Date.Date -> Html msg
datetimeView datetime =
    span
        [ style
            [ ( "margin-right", "0.5em" )
            , ( "font-size", "80%" )
            , ( "vertical-align", "text-top" )
            , ( "color", "#aaa" )
            ]
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
                ]
            ]

        detailView =
            if isSelected then
                [ div
                    [ style
                        [ ( "padding", "1em" )
                        , ( "font-size", "90%" )
                        ]
                    , class "animated fadeIn"
                    ]
                    [ div []
                        [ datetimeView item.updated_at
                        , linkIconView FIcons.externalLink (Just item.link)
                        ]
                    , Markdown.toHtml [] item.description
                    ]
                ]
            else
                []
    in
        div
            [ style
                [ ( "borderBottom", "1px solid #ddd" )
                , ( "padding", "0.5em 0" )
                ]
            ]
            (titleView ++ detailView)


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
                            , linkIconView FIcons.externalLink feed.link
                            ]
                        ]
                    , p [] [ text feed.description ]
                    ]

        titleDiv =
            div
                [ style
                    [ ( "border-bottom", "1px solid #999" )
                    ]
                ]
                titleContent

        itemsList =
            case items of
                RemoteData.NotAsked ->
                    text ""

                RemoteData.Loading ->
                    loadingView

                RemoteData.Failure err ->
                    text ("Error: " ++ toString err)

                RemoteData.Success resp ->
                    resp.results
                        |> List.map (\item -> feedItemView (SelectFeedItem item.id) selectedId item)
                        |> div [ class "animated fadeIn" ]
    in
        div
            [ style [ ( "padding", "0 0.5em" ) ] ]
            [ titleDiv
            , itemsList
            ]


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
            [ model.feeds
                |> feedsView (model.selectedFeed |> Maybe.map .id)
            ]
        , div
            [ class "pure-u-3-4"
            , style
                [ ( "height", "100%" )
                ]
            ]
            [ model.feedItems
                |> feedItemsView model.selectedFeed model.selectedFeedItemId
            ]
        ]

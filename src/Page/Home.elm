module Page.Home exposing (Model, Msg, init, update, view, subscriptions)

import Html
import Css exposing (..)
import Css.Colors as Colors
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, class, classList, href, rel, title)
import Html.Styled.Events exposing (onClick)
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
            , css
                [ overflow hidden
                , textOverflow ellipsis
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
        [ css
            [ width (pct 100)
            , textAlign center
            ]
        , class "animated rotateIn"
        ]
        [ Icons.loader
            |> Icons.toHtmlStyled []
        ]


linkIconView : Icons.Icon -> Maybe String -> Html msg
linkIconView icon url =
    a
        [ href (url |> Maybe.withDefault "#")
        , Html.Styled.Attributes.target "_blank"
        , rel "noopener noreferrer"
        , css
            [ color Colors.black
            , paddingRight (Css.em 0.5)
            ]
        , classList [ ( "hidden", url == Nothing ) ]
        ]
        [ icon
            |> Icons.withSize 1
            |> Icons.withSizeUnit "em"
            |> Icons.toHtmlStyled []
        ]


datetimeView : Date.Date -> Html msg
datetimeView datetime =
    span
        [ css
            [ marginRight (Css.em 0.5)
            , fontSize (pct 80)
            , verticalAlign textTop
            , color (hex "#aaa")
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

        titleView =
            [ div
                [ onClick onClickMsg ]
                [ span
                    [ css
                        [ cursor pointer
                        ]
                    ]
                    [ text item.title ]
                ]
            ]

        detailView =
            if isSelected then
                [ div
                    [ css
                        [ padding (Css.em 1)
                        , fontSize (pct 90)
                        ]
                    , class "animated fadeIn"
                    ]
                    [ div []
                        [ datetimeView item.updated_at
                        , linkIconView Icons.externalLink (Just item.link)
                        ]
                    , item.description
                        |> Markdown.toHtml []
                        |> fromUnstyled
                    ]
                ]
            else
                []
    in
        div
            [ css
                [ borderBottom3 (px 1) solid (hex "#ddd")
                , padding2 (Css.em 0.5) zero
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
                            , linkIconView Icons.externalLink feed.link
                            ]
                        ]
                    , p
                        [ css
                            [ fontStyle italic
                            , fontSize (pct 80)
                            ]
                        ]
                        [ text feed.description ]
                    ]

        titleDiv =
            div
                [ css
                    [ borderBottom3 (px 1) solid (hex "#999")
                    ]
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
                        []
                        [ titleDiv
                        , resp.results
                            |> List.map (\item -> feedItemView (SelectFeedItem item.id) selectedId item)
                            |> div [ class "animated fadeIn" ]
                        ]
    in
        div
            [ css [ padding2 zero (Css.em 0.5) ] ]
            [ content
            ]


styledView : Model -> Html Msg
styledView model =
    div
        [ class "pure-g" ]
        [ div
            [ class "pure-u-1-4"
            , css
                [ height (pct 100)
                ]
            ]
            [ model.feeds
                |> feedsView (model.selectedFeed |> Maybe.map .id)
            ]
        , div
            [ class "pure-u-3-4"
            , css
                [ height (pct 100)
                ]
            ]
            [ model.feedItems
                |> feedItemsView model.selectedFeed model.selectedFeedItemId
            ]
        ]


view =
    styledView >> toUnstyled

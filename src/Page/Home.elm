module Page.Home exposing (Model, Msg, init, subscriptions, update, view)

import Api
import Browser.Dom as Dom
import Data.Feed as Feed
import Data.Feed.Item as FeedItem
import Html exposing (..)
import Html.Keyed as Keyed
import Html.Lazy as Lazy
import Html.Attributes
    exposing
        ( class
        , classList
        , href
        , id
        , rel
        , style
        , target
        , title
        )
import Html.Events exposing (onClick)
import Http
import HttpBuilder
import Icons
import Json.Decode as Decode
import Markdown
import RemoteData exposing (WebData)
import String
import Task


type alias Model =
    { feeds : Api.FeedsResponse
    , feedItems : Api.FeedItemsResponse
    , selectedFeed : Maybe Feed.Feed
    , selectedFeedItemId : Maybe FeedItem.FeedItemId
    , menuActive : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { feeds = RemoteData.Loading
      , feedItems = RemoteData.Loading
      , selectedFeed = Nothing
      , selectedFeedItemId = Nothing
      , menuActive = False
      }
    , Cmd.batch
        [ Api.listFeedsRequest 1 FeedsResponse
        , Api.listFeedItemsRequest 1 Nothing FeedItemsResponse
        ]
    )


type Msg
    = Noop
    | FeedsResponse Api.FeedsResponse
    | FeedItemsResponse Api.FeedItemsResponse
    | LoadMoreItems (Maybe Int)
    | SelectFeed (Maybe Feed.FeedId)
    | SelectFeedItem FeedItem.FeedItemId
    | ToggleMenuActive


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model
            , Cmd.none
            )

        ToggleMenuActive ->
            ( { model | menuActive = not model.menuActive }
            , Cmd.none
            )

        FeedsResponse resp ->
            ( { model | feeds = resp }
            , Cmd.none
            )

        FeedItemsResponse resp ->
            ( { model | feedItems = resp }
            , Cmd.none
            )

        LoadMoreItems maybePage ->
            let
                page =
                    maybePage
                        |> Maybe.withDefault 1

                feedId =
                    model.selectedFeed
                        |> Maybe.map .id
            in
                ( { model
                    | feedItems = RemoteData.Loading
                  }
                , Api.listFeedItemsRequest page feedId FeedItemsResponse
                )

        SelectFeed maybeFeedId ->
            let
                selectedFeed =
                    case maybeFeedId of
                        Just feedId ->
                            model.feeds
                                |> RemoteData.toMaybe
                                |> Maybe.andThen
                                    (.results
                                        >> List.filter (\f -> f.id == feedId)
                                        >> List.head
                                    )

                        Nothing ->
                            Nothing
            in
                ( { model
                    | selectedFeed = selectedFeed
                    , selectedFeedItemId = Nothing
                    , menuActive = False
                    , feedItems = RemoteData.Loading
                  }
                , Api.listFeedItemsRequest 1 maybeFeedId FeedItemsResponse
                )

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

                cmd =
                    case newSelectedId of
                        Nothing ->
                            Cmd.none

                        Just id ->
                            jumpTo id
            in
                ( { model | selectedFeedItemId = newSelectedId }
                , cmd
                )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


jumpTo : FeedItem.FeedItemId -> Cmd Msg
jumpTo itemId =
    Dom.getElement (feedItemElementId itemId)
        |> Task.andThen
            (\info -> Dom.setViewport info.viewport.x info.element.y)
        |> Task.attempt (\_ -> Noop)


feedView : Maybe Feed.FeedId -> Maybe Feed.FeedId -> String -> Html Msg
feedView selectedFeedId feedId feedTitle =
    li
        [ class "pure-menu-item"
        , classList
            [ ( "pure-menu-selected"
              , selectedFeedId == feedId
              )
            ]
        , onClick (SelectFeed feedId)
        ]
        [ a
            [ href "#"
            , title feedTitle
            , class "pure-menu-link"
            , style "overflow" "hidden"
            , style "text-overflow" "ellipsis"
            ]
            [ text feedTitle ]
        ]


feedsView : Maybe Feed.FeedId -> Api.FeedsResponse -> Html Msg
feedsView selectedFeedId feeds =
    let
        content =
            case feeds of
                RemoteData.NotAsked ->
                    text "Initializing.."

                RemoteData.Loading ->
                    loadingView

                RemoteData.Failure err ->
                    text "Network Error"

                RemoteData.Success resp ->
                    let
                        latestLink =
                            feedView Nothing Nothing "Latest"

                        feedLinks =
                            resp.results
                                |> List.map (\f -> feedView selectedFeedId (Just f.id) f.title)
                    in
                        ul [ class "pure-menu-list" ] (latestLink :: feedLinks)
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


datetimeView : String -> Html msg
datetimeView datetime =
    span
        [ class "datetime"
        ]
        [ text datetime
        ]


markdownOptions : Markdown.Options
markdownOptions =
    let
        defaultOptions =
            Markdown.defaultOptions
    in
        { defaultOptions | sanitize = False }


feedItemElementId : FeedItem.FeedItemId -> String
feedItemElementId itemId =
    "item-" ++ FeedItem.idToString itemId


feedItemView : Msg -> Bool -> FeedItem.FeedItem -> Html Msg
feedItemView onClickMsg isSelected item =
    let
        titleView =
            div
                [ onClick onClickMsg
                , class "title"
                , classList [ ( "selected", isSelected ) ]
                ]
                [ text item.title ]

        detailView =
            if isSelected then
                div
                    [ class "body"
                    ]
                    [ div []
                        [ datetimeView item.updated_at
                        , linkIconView Icons.externalLink (Just item.link)
                        ]
                    , item.description
                        |> Markdown.toHtmlWith markdownOptions []
                    ]
            else
                text ""
    in
        li
            [ class "feed-item"
            , id (feedItemElementId item.id)
            ]
            [ titleView, detailView ]


keyedFeedItemView : Msg -> Maybe FeedItem.FeedItemId -> FeedItem.FeedItem -> ( String, Html Msg )
keyedFeedItemView onClickMsg selectedId item =
    let
        isSelected =
            selectedId
                |> Maybe.map ((==) item.id)
                |> Maybe.withDefault False
    in
        ( FeedItem.idToString item.id, Lazy.lazy3 feedItemView onClickMsg isSelected item )


paginationButtonView : String -> Maybe Int -> Html Msg
paginationButtonView buttonText maybePage =
    button
        [ class "loadButton pure-button"
        , classList
            [ ( "pure-button-disabled", maybePage == Nothing )
            ]
        , onClick (LoadMoreItems maybePage)
        ]
        [ text buttonText ]


feedItemsView : Maybe Feed.Feed -> Maybe FeedItem.FeedItemId -> Api.FeedItemsResponse -> Html Msg
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
    in
        case items of
            RemoteData.NotAsked ->
                text ""

            RemoteData.Loading ->
                loadingView

            RemoteData.Failure err ->
                text "Network Error"

            RemoteData.Success resp ->
                let
                    currentPage =
                        Api.currentPage resp
                in
                    div
                        [ class "feed-items" ]
                        [ titleDiv
                        , Keyed.ol
                            [ class "animated fadeIn"
                            ]
                            (resp.results
                                |> List.map
                                    (\item -> keyedFeedItemView (SelectFeedItem item.id) selectedId item)
                            )
                        , div [ class "pagination" ]
                            [ paginationButtonView "Prev" resp.previous
                            , paginationButtonView (String.fromInt currentPage) (Just currentPage)
                            , paginationButtonView "Next" resp.next
                            ]
                        ]


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
                    |> Lazy.lazy2 feedsView (model.selectedFeed |> Maybe.map .id)
                ]
            , div
                -- main
                [ id "main" ]
                [ div
                    [ class "overlay"
                    , classList [ ( "active", model.menuActive ) ]
                    , onClick ToggleMenuActive
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
                        |> Lazy.lazy3 feedItemsView model.selectedFeed model.selectedFeedItemId
                    ]
                ]
            ]

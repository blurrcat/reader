module Page.Home exposing (Model, Msg, init, subscriptions, update, view)

import Api
import Data.Feed as Feed
import Data.Feed.Item as FeedItem
import Time
import Html exposing (..)
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


type alias Model =
    { feeds : Api.FeedsResponse
    , feedItems : Api.FeedItemsResponse
    , selectedFeed : Maybe Feed.Feed
    , selectedFeedItemId : Maybe FeedItem.FeedItemId
    , menuActive : Bool
    , currentPage : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { feeds = RemoteData.Loading
      , feedItems = RemoteData.Loading
      , selectedFeed = Nothing
      , selectedFeedItemId = Nothing
      , menuActive = False
      , currentPage = 1
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
    | SelectFeed Feed.FeedId
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
                ( { model | feedItems = RemoteData.Loading }, Api.listFeedItemsRequest page feedId FeedItemsResponse )

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
                ( { model
                    | selectedFeed = selectedFeed
                    , menuActive = False
                    , feedItems = RemoteData.Loading
                  }
                , Api.listFeedItemsRequest 1 (Just feedId) FeedItemsResponse
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
            in
                ( { model | selectedFeedItemId = newSelectedId }
                , Cmd.none
                )


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
            , style "overflow" "hidden"
            , style "text-overflow" "ellipsis"
            ]
            [ text feed.title ]
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
                        |> Markdown.toHtmlWith markdownOptions []
                    ]
                ]
            else
                []
    in
        div
            [ class "feed-item"
            ]
            (titleView :: detailView)


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

        content =
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
                            , div [ class "animated fadeIn" ]
                                (resp.results
                                    |> List.map
                                        (\item -> feedItemView (SelectFeedItem item.id) selectedId item)
                                )
                            , div [ class "pagination" ]
                                [ paginationButtonView "Prev" resp.previous
                                , paginationButtonView (String.fromInt currentPage) (Just currentPage)
                                , paginationButtonView "Next" resp.next
                                ]
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

module Page.Home exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import RemoteData exposing (WebData)
import Json.Decode as Decode
import Data.Feed as Feed
import HttpBuilder
import Api


type alias Feeds =
    Api.ListResponse Feed.Feed


type alias Model =
    { feeds : WebData Feeds
    , selectedFeedId : Maybe Feed.FeedId
    }


getFeeds page =
    (Api.list
        [ ( "page", toString page ) ]
        Feed.decoder
        "/feeds/"
    )
        |> RemoteData.sendRequest
        |> Cmd.map FeedsResponse


init : ( Model, Cmd Msg )
init =
    { feeds = RemoteData.Loading
    , selectedFeedId = Nothing
    }
        ! [ getFeeds 1 ]


type Msg
    = Noop
    | FeedsResponse (WebData Feeds)
    | SelectFeed Feed.FeedId


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            model ! []

        FeedsResponse resp ->
            { model | feeds = resp } ! []

        SelectFeed feedId ->
            { model | selectedFeedId = Just feedId } ! []


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
            [ text "main" ]
        ]

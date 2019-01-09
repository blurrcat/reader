module Main exposing (main)

import Api
import Browser exposing (Document)
import Browser.Navigation as Nav
import Url exposing (Url)
import Html
import Pages.Home as Home
import Pages.Login as Login
import Route
import Session exposing (Session, navKey)
import Json.Encode as Encode


main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlClicked
        }



-- MODEL


type alias Flags =
    Encode.Value


type Msg
    = Noop
    | GotHomeMsg Home.Msg
    | GotLoginMsg Login.Msg
    | UrlClicked Browser.UrlRequest
    | UrlChanged Url


type Model
    = NotFound Session
    | Home Home.Model
    | Login Login.Model


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        session =
            Session.new (Api.decodeCred flags) key
    in
        changeRouteTo
            (Route.fromUrl url)
            (NotFound session)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        session =
            toSession model
    in
        case ( msg, model ) of
            -- routing
            ( UrlChanged url, _ ) ->
                changeRouteTo (Route.fromUrl url) model

            ( UrlClicked urlRequest, _ ) ->
                ( model
                , handleUrlRequest (navKey session) urlRequest
                )

            ( GotHomeMsg subMsg, Home subModel ) ->
                Home.update subMsg subModel
                    |> updateWith Home GotHomeMsg model

            ( GotLoginMsg subMsg, Login subModel ) ->
                Login.update subMsg subModel
                    |> updateWith Login GotLoginMsg model

            ( someMsg, someModel ) ->
                -- disgard message that arrive on the wrong page
                ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Home subModel ->
            Sub.map GotHomeMsg (Home.subscriptions subModel)

        Login subModel ->
            Sub.map GotLoginMsg (Login.subscriptions subModel)

        NotFound _ ->
            Sub.none



-- VIEW


viewPage : (subModel -> Document subMsg) -> (subMsg -> Msg) -> subModel -> Document Msg
viewPage pageView toMsg subModel =
    let
        document =
            pageView subModel
    in
        { title = document.title
        , body = List.map (Html.map toMsg) document.body
        }


view : Model -> Document Msg
view model =
    case model of
        Home subModel ->
            viewPage Home.view GotHomeMsg subModel

        Login subModel ->
            viewPage Login.view GotLoginMsg subModel

        NotFound _ ->
            { title = "404"
            , body = [ Html.text "404 NOT FOUND" ]
            }



-- INTERNAL


toSession model =
    case model of
        NotFound session ->
            session

        Home subModel ->
            subModel.session

        Login subModel ->
            subModel.session


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


changeRouteTo : Maybe Route.Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
        case maybeRoute of
            Nothing ->
                ( model, Cmd.none )

            Just route ->
                case route of
                    Route.Home ->
                        Home.init session
                            |> updateWith Home GotHomeMsg model

                    Route.Login ->
                        Login.init session
                            |> updateWith Login GotLoginMsg model


handleUrlRequest : Nav.Key -> Browser.UrlRequest -> Cmd a
handleUrlRequest navKey urlRequest =
    case urlRequest of
        Browser.Internal url ->
            Nav.pushUrl navKey (Url.toString url)

        Browser.External href ->
            Nav.load href

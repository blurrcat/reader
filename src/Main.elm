module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Url exposing (Url)
import Html
import Pages.Home as Home
import Pages.Login as Login
import Route


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


type alias Flag =
    {}


type Msg
    = Noop
    | GotHomeMsg Home.Msg
    | GotLoginMsg Login.Msg
    | UrlClicked Browser.UrlRequest
    | UrlChanged Url


type PageModel
    = NotFound
    | Home Home.Model
    | Login Login.Model


type alias Model =
    { pageModel : PageModel
    , navKey : Nav.Key
    }


updateWith : (subModel -> PageModel) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toPageModel toMsg model ( subModel, subCmd ) =
    ( { model
        | pageModel = toPageModel subModel
      }
    , Cmd.map toMsg subCmd
    )


changeRouteTo : Maybe Route.Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( model, Cmd.none )

        Just route ->
            case route of
                Route.Home ->
                    Home.init
                        |> updateWith Home GotHomeMsg model

                Route.Login ->
                    Login.init
                        |> updateWith Login GotLoginMsg model


init : Flag -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    changeRouteTo
        (Route.fromUrl url)
        { pageModel = NotFound
        , navKey = key
        }



-- UPDATE


handleUrlRequest : Nav.Key -> Browser.UrlRequest -> Cmd a
handleUrlRequest navKey urlRequest =
    case urlRequest of
        Browser.Internal url ->
            Nav.pushUrl navKey (Url.toString url)

        Browser.External href ->
            Nav.load href


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.pageModel ) of
        -- routing
        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( UrlClicked urlRequest, _ ) ->
            ( model
            , handleUrlRequest model.navKey urlRequest
            )

        ( GotHomeMsg subMsg, Home subModel ) ->
            Home.update subMsg subModel
                |> updateWith Home GotHomeMsg model

        ( _, _ ) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.pageModel of
        Home subModel ->
            Sub.map GotHomeMsg (Home.subscriptions subModel)

        Login subModel ->
            Sub.map GotLoginMsg (Login.subscriptions subModel)

        NotFound ->
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
    case model.pageModel of
        Home subModel ->
            viewPage Home.view GotHomeMsg subModel

        Login subModel ->
            viewPage Login.view GotLoginMsg subModel

        NotFound ->
            { title = "404"
            , body = [ Html.text "404 NOT FOUND" ]
            }

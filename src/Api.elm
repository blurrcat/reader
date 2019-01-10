port module Api
    exposing
        ( HttpResultMsg
        , Cred
        , storeCred
        , decodeCred
        , onCredChanges
        , login
        , logout
        , get
        , post
        )

import Api.Endpoint as Endpoint exposing (Endpoint)
import Http exposing (Body, jsonBody)
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)


type Cred
    = Cred String


credDecoder : Decoder Cred
credDecoder =
    Decode.succeed Cred
        |> required "token" Decode.string


type alias HttpResultMsg a msg =
    Result Http.Error a -> msg


port onStoreChanges : (Value -> msg) -> Sub msg


port storeCache : Maybe Value -> Cmd msg


storeCred : Cred -> Cmd msg
storeCred (Cred token) =
    let
        json =
            Encode.object
                [ ( "token", Encode.string token ) ]
    in
        storeCache (Just json)


decodeCred : Encode.Value -> Maybe Cred
decodeCred value =
    Decode.decodeValue Decode.string value
        |> Result.andThen (Decode.decodeString credDecoder)
        |> Result.toMaybe


onCredChanges : (Maybe Cred -> msg) -> Sub msg
onCredChanges credToMsg =
    let
        valueToMsg =
            Decode.decodeValue credDecoder >> Result.toMaybe >> credToMsg
    in
        onStoreChanges valueToMsg



-- API


login : String -> String -> HttpResultMsg Cred msg -> Cmd msg
login username password msg =
    let
        body =
            Encode.object
                [ ( "username", Encode.string username )
                , ( "password", Encode.string password )
                ]
                |> jsonBody
    in
        post Endpoint.tokens body Nothing msg credDecoder


logout : Cmd msg
logout =
    storeCache Nothing



-- HTTP methods


get : Endpoint -> Maybe Cred -> HttpResultMsg a msg -> Decoder a -> Cmd msg
get url maybeCred msg decoder =
    Endpoint.request
        { method = "GET"
        , url = url
        , headers = authHeader maybeCred
        , body = Http.emptyBody
        , expect = Http.expectJson msg decoder
        , timeout = Nothing
        , tracker = Nothing
        }


post : Endpoint -> Body -> Maybe Cred -> HttpResultMsg a msg -> Decoder a -> Cmd msg
post url body maybeCred msg decoder =
    Endpoint.request
        { method = "POST"
        , url = url
        , headers = authHeader maybeCred
        , body = body
        , expect = Http.expectJson msg decoder
        , timeout = Nothing
        , tracker = Nothing
        }


authHeader maybeCred =
    case maybeCred of
        Just (Cred token) ->
            [ Http.header "Authorization" ("Bearer " ++ token) ]

        Nothing ->
            []

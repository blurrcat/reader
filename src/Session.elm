module Session exposing (Session, new, cred, navKey, changes)

import Api exposing (Cred)
import Browser.Navigation as Nav


type Session
    = LoggedIn Nav.Key Cred
    | Anonymous Nav.Key


new : Maybe Cred -> Nav.Key -> Session
new maybeCred key =
    case maybeCred of
        Just cred_ ->
            LoggedIn key cred_

        Nothing ->
            Anonymous key


cred : Session -> Maybe Cred
cred session =
    case session of
        LoggedIn _ cred_ ->
            Just cred_

        Anonymous _ ->
            Nothing


navKey : Session -> Nav.Key
navKey session =
    case session of
        LoggedIn key _ ->
            key

        Anonymous key ->
            key


changes : (Session -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
    Api.onCredChanges (\maybeCred -> toMsg (new maybeCred key))

port module Main exposing (..)

import Flags exposing (Flags)
import Init exposing (init)
import Model exposing (Model)
import Msg exposing (Msg(OnLocationChange))
import Navigation
import Subscriptions exposing (subscriptions)
import Update exposing (update)
import View exposing (view)


{-| The entry point to the elm application. The navigation module allows us to use the `urlUpdate` field so we can
essentially subscribe to url changes.
-}
main : Program Flags Model Msg
main =
    Navigation.programWithFlags
        OnLocationChange
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

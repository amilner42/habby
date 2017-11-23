module Init exposing (init)

import DefaultModel exposing (..)
import Flags exposing (Flags)
import Model exposing (..)
import Msg exposing (..)
import Navigation
import Update exposing (..)


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    ( {}, Cmd.none )

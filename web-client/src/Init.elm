module Init exposing (init)

import Api
import DefaultModel exposing (defaultModel)
import Flags exposing (Flags)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Navigation


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    ( defaultModel flags
    , Api.queryHabitsAndHabitData flags.apiBaseUrl OnApiError OnGetHabitsAndHabitDataSuccess
    )

module Init exposing (init)

import Api
import Flags exposing (Flags)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Navigation
import RemoteData


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init { apiBaseUrl } location =
    ( { apiBaseUrl = apiBaseUrl, allHabitData = RemoteData.Loading, allHabits = RemoteData.Loading }
    , Api.queryHabitsAndHabitData apiBaseUrl OnGetHabitsAndHabitDataFailure OnGetHabitsAndHabitDataSuccess
    )

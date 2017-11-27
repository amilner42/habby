module Init exposing (init)

import Api
import Date
import Flags exposing (Flags)
import Model exposing (Model)
import Models.YmdDate as YmdDate
import Msg exposing (Msg(..))
import Navigation
import RemoteData


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init { apiBaseUrl, currentTime } location =
    ( { ymd = currentTime |> Date.fromTime |> YmdDate.fromDate
      , apiBaseUrl = apiBaseUrl
      , allHabitData = RemoteData.Loading
      , allHabits = RemoteData.Loading
      }
    , Api.queryHabitsAndHabitData apiBaseUrl OnGetHabitsAndHabitDataFailure OnGetHabitsAndHabitDataSuccess
    )

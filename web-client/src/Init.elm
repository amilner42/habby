module Init exposing (init)

import Api
import Date
import Dict
import Flags exposing (Flags)
import Model exposing (Model)
import Models.Habit as Habit
import Models.YmdDate as YmdDate
import Msg exposing (Msg(..))
import Navigation
import RemoteData


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init { apiBaseUrl, currentTime } location =
    ( { ymd = currentTime |> Date.fromTime |> YmdDate.fromDate
      , apiBaseUrl = apiBaseUrl
      , editingTodayHabitAmount = Dict.empty
      , editingHistoryHabitAmount = Dict.empty
      , allHabitData = RemoteData.Loading
      , allHabits = RemoteData.Loading
      , addHabit = Habit.initAddHabitData
      , openTodayViewer = True
      , openHistoryViewer = False
      , historyViewerDateInput = ""
      , historyViewerSelectedDate = Nothing
      }
    , Api.queryHabitsAndHabitData apiBaseUrl OnGetHabitsAndHabitDataFailure OnGetHabitsAndHabitDataSuccess
    )

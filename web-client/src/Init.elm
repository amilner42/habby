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
    let
        ymd =
            currentTime |> Date.fromTime |> YmdDate.fromDate
    in
        ( { ymd = ymd
          , apiBaseUrl = apiBaseUrl
          , editHabitDict = Dict.empty
          , editingTodayHabitAmount = Dict.empty
          , editingHistoryHabitAmount = Dict.empty
          , allHabitData = RemoteData.Loading
          , allHabits = RemoteData.Loading
          , allFrequencyStats = RemoteData.Loading
          , addHabit = Habit.initAddHabitData
          , openTodayViewer = True
          , openHistoryViewer = False
          , historyViewerDateInput = ""
          , historyViewerSelectedDate = Nothing
          , historyViewerFrequencyStats = RemoteData.NotAsked
          }
        , Api.queryHabitsAndHabitDataAndFrequencyStats
            ymd
            apiBaseUrl
            OnGetHabitsAndHabitDataAndFrequencyStatsFailure
            OnGetHabitsAndHabitDataAndFrequencyStatsSuccess
        )

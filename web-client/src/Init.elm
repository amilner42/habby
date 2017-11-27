module Init exposing (init)

import Api
import Date
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
      , allHabitData = RemoteData.Loading
      , allHabits = RemoteData.Loading
      , addHabit =
            { openView = False
            , kind = Habit.GoodHabitKind
            , name = ""
            , description = ""
            , goodHabitTime = Habit.Anytime
            , unitNameSingular = ""
            , unitNamePlural = ""
            , frequencyKind = Habit.TotalWeekFrequencyKind
            , timesPerWeek = Nothing
            , mondayTimes = Nothing
            , tuesdayTimes = Nothing
            , wednesdayTimes = Nothing
            , thursdayTimes = Nothing
            , fridayTimes = Nothing
            , saturdayTimes = Nothing
            , sundayTimes = Nothing
            , times = Nothing
            , days = Nothing
            }
      }
    , Api.queryHabitsAndHabitData apiBaseUrl OnGetHabitsAndHabitDataFailure OnGetHabitsAndHabitDataSuccess
    )

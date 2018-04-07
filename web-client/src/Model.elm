module Model exposing (..)

import Dict
import Models.ApiError as ApiError
import Models.Habit as Habit
import Models.HabitData as HabitData
import Models.FrequencyStats as FrequencyStats
import Models.YmdDate as YmdDate
import RemoteData


type alias Model =
    { ymd : YmdDate.YmdDate
    , apiBaseUrl : String
    , editHabitDict : Dict.Dict String Habit.EditHabit
    , editingTodayHabitAmount : Dict.Dict String Int
    , editingHistoryHabitAmount : Dict.Dict String (Dict.Dict String Int)
    , allHabits : RemoteData.RemoteData ApiError.ApiError (List Habit.Habit)
    , allHabitData : RemoteData.RemoteData ApiError.ApiError (List HabitData.HabitData)
    , allFrequencyStats : RemoteData.RemoteData ApiError.ApiError (List FrequencyStats.FrequencyStats)
    , addHabit : Habit.AddHabitInputData
    , openTodayViewer : Bool
    , openHistoryViewer : Bool
    , historyViewerDateInput : String
    , historyViewerSelectedDate : Maybe YmdDate.YmdDate
    , historyViewerFrequencyStats : RemoteData.RemoteData ApiError.ApiError (List FrequencyStats.FrequencyStats)
    }

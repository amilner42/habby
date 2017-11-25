module Model exposing (..)

import Models.ApiError as ApiError
import Models.Habit as Habit
import Models.HabitData as HabitData
import RemoteData


type alias Model =
    { apiBaseUrl : String
    , allHabits : RemoteData.RemoteData ApiError.ApiError (List Habit.Habit)
    , allHabitData : RemoteData.RemoteData ApiError.ApiError (List HabitData.HabitData)
    }

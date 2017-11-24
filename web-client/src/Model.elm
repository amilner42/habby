module Model exposing (..)

import Models.Habit as Habit
import Models.HabitData as HabitData


type alias Model =
    { apiBaseUrl : String
    , allHabits : List Habit.Habit
    , allHabitData : List HabitData.HabitData
    }

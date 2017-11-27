module Msg exposing (..)

import Api
import Models.ApiError exposing (ApiError)
import Navigation
import Time


type Msg
    = OnLocationChange Navigation.Location
    | TickMinute Time.Time
    | OnGetHabitsAndHabitDataFailure ApiError
    | OnGetHabitsAndHabitDataSuccess Api.HabitsAndHabitData

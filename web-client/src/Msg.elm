module Msg exposing (..)

import Api
import Models.ApiError exposing (ApiError)
import Navigation


type Msg
    = OnLocationChange Navigation.Location
    | OnGetHabitsAndHabitDataFailure ApiError
    | OnGetHabitsAndHabitDataSuccess Api.HabitsAndHabitData

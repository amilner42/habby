module Update exposing (..)

import Model exposing (Model)
import Msg exposing (Msg(..))
import RemoteData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnGetHabitsAndHabitDataFailure apiError ->
            ( { model
                | allHabits = RemoteData.Failure apiError
                , allHabitData = RemoteData.Failure apiError
              }
            , Cmd.none
            )

        OnGetHabitsAndHabitDataSuccess { habits, habitData } ->
            ( { model
                | allHabits = RemoteData.Success habits
                , allHabitData = RemoteData.Success habitData
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )

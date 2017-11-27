module Update exposing (..)

import Date
import Model exposing (Model)
import Models.YmdDate as YmdDate
import Msg exposing (Msg(..))
import RemoteData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLocationChange location ->
            -- TODO
            ( model, Cmd.none )

        TickMinute time ->
            ( { model | ymd = time |> Date.fromTime |> YmdDate.fromDate }, Cmd.none )

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

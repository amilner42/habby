module Update exposing (..)

import Model exposing (Model)
import Msg exposing (Msg(..))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnGetHabitsAndHabitDataSuccess { habits, habitData } ->
            ( { model | allHabits = habits, allHabitData = habitData }, Cmd.none )

        _ ->
            ( model, Cmd.none )

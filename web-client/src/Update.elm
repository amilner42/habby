module Update exposing (..)

import Api
import Date
import DefaultServices.Util as Util
import Dict
import Model exposing (Model)
import Models.Habit as Habit
import Models.YmdDate as YmdDate
import Msg exposing (Msg(..))
import RemoteData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateAddHabit updater =
            { model | addHabit = updater model.addHabit }
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

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

        OnOpenAddHabit ->
            ( updateAddHabit (\addHabit -> { addHabit | openView = True }), Cmd.none )

        OnCancelAddHabit ->
            ( updateAddHabit (\addHabit -> { addHabit | openView = False }), Cmd.none )

        OnSelectAddHabitKind habitKind ->
            ( updateAddHabit (\addHabit -> { addHabit | kind = habitKind }), Cmd.none )

        OnAddHabitNameInput habitName ->
            ( updateAddHabit (\addHabit -> { addHabit | name = habitName }), Cmd.none )

        OnAddHabitDescriptionInput habitDescription ->
            ( updateAddHabit (\addHabit -> { addHabit | description = habitDescription }), Cmd.none )

        OnSelectAddGoodHabitTime goodHabitTime ->
            ( updateAddHabit (\addHabit -> { addHabit | goodHabitTime = goodHabitTime }), Cmd.none )

        OnAddHabitUnitNameSingularInput unitNameSingular ->
            ( updateAddHabit (\addHabit -> { addHabit | unitNameSingular = unitNameSingular }), Cmd.none )

        OnAddHabitUnitNamePluralInput unitNamePlural ->
            ( updateAddHabit (\addHabit -> { addHabit | unitNamePlural = unitNamePlural }), Cmd.none )

        OnAddHabitSelectFrequencyKind frequencyKind ->
            ( updateAddHabit (\addHabit -> { addHabit | frequencyKind = frequencyKind }), Cmd.none )

        OnAddHabitTimesPerWeekInput timesPerWeek ->
            ( updateAddHabit (\addHabit -> { addHabit | timesPerWeek = extractInt timesPerWeek addHabit.timesPerWeek })
            , Cmd.none
            )

        OnAddHabitSpecificDayMondayInput mondayTimes ->
            ( updateAddHabit (\addHabit -> { addHabit | mondayTimes = extractInt mondayTimes addHabit.mondayTimes })
            , Cmd.none
            )

        OnAddHabitSpecificDayTuesdayInput tuesdayTimes ->
            ( updateAddHabit (\addHabit -> { addHabit | tuesdayTimes = extractInt tuesdayTimes addHabit.tuesdayTimes })
            , Cmd.none
            )

        OnAddHabitSpecificDayWednesdayInput wednesdayTimes ->
            ( updateAddHabit (\addHabit -> { addHabit | wednesdayTimes = extractInt wednesdayTimes addHabit.wednesdayTimes })
            , Cmd.none
            )

        OnAddHabitSpecificDayThursdayInput thursdayTimes ->
            ( updateAddHabit (\addHabit -> { addHabit | thursdayTimes = extractInt thursdayTimes addHabit.thursdayTimes })
            , Cmd.none
            )

        OnAddHabitSpecificDayFridayInput fridayTimes ->
            ( updateAddHabit (\addHabit -> { addHabit | fridayTimes = extractInt fridayTimes addHabit.fridayTimes })
            , Cmd.none
            )

        OnAddHabitSpecificDaySaturdayInput saturdayTimes ->
            ( updateAddHabit (\addHabit -> { addHabit | saturdayTimes = extractInt saturdayTimes addHabit.saturdayTimes })
            , Cmd.none
            )

        OnAddHabitSpecificDaySundayInput sundayTimes ->
            ( updateAddHabit (\addHabit -> { addHabit | sundayTimes = extractInt sundayTimes addHabit.sundayTimes })
            , Cmd.none
            )

        OnAddHabitTimesInput times ->
            ( updateAddHabit (\addHabit -> { addHabit | times = extractInt times addHabit.times })
            , Cmd.none
            )

        OnAddHabitDaysInput days ->
            ( updateAddHabit (\addHabit -> { addHabit | days = extractInt days addHabit.days })
            , Cmd.none
            )

        AddHabit createHabitData ->
            ( model
            , Api.mutationAddHabit createHabitData model.apiBaseUrl OnAddHabitFailure OnAddHabitSuccess
            )

        OnAddHabitFailure apiError ->
            -- TODO
            ( model, Cmd.none )

        OnAddHabitSuccess habit ->
            ( { model
                | allHabits = RemoteData.map (\allHabits -> allHabits ++ [ habit ]) model.allHabits
                , addHabit = Habit.initAddHabitData
              }
            , Cmd.none
            )

        OnHabitDataInput habitID newVal ->
            let
                newEditingTodayHabitAmount amount =
                    model.editingTodayHabitAmount
                        |> Dict.update habitID (always <| amount)
            in
            if String.isEmpty newVal then
                ( { model | editingTodayHabitAmount = newEditingTodayHabitAmount Nothing }, Cmd.none )
            else
                case String.toInt newVal of
                    Result.Err _ ->
                        ( model, Cmd.none )

                    Result.Ok newInt ->
                        ( { model | editingTodayHabitAmount = newEditingTodayHabitAmount <| Just newInt }, Cmd.none )

        SetHabitData ymd habitId newVal ->
            case newVal of
                Nothing ->
                    ( model, Cmd.none )

                Just newVal ->
                    ( model
                    , Api.mutationSetHabitData
                        ymd
                        habitId
                        newVal
                        model.apiBaseUrl
                        OnSetHabitDataFailure
                        OnSetHabitDataSuccess
                    )

        OnSetHabitDataFailure apiError ->
            -- TODO
            ( model, Cmd.none )

        OnSetHabitDataSuccess updatedHabitDatum ->
            ( { model
                | allHabitData =
                    RemoteData.map
                        (\allHabitData ->
                            Util.replaceOrAdd allHabitData (.id >> (==) updatedHabitDatum.id) updatedHabitDatum
                        )
                        model.allHabitData
                , editingTodayHabitAmount =
                    Dict.update updatedHabitDatum.habitId (always Nothing) model.editingTodayHabitAmount
              }
            , Cmd.none
            )

        OnToggleHistoryViewer ->
            let
                historyViewer =
                    model.historyViewer
            in
            ( { model | historyViewer = { historyViewer | openView = not historyViewer.openView } }, Cmd.none )

        OnToggleTodayViewer ->
            let
                todayViewer =
                    model.todayViewer
            in
            ( { model | todayViewer = { todayViewer | openView = not todayViewer.openView } }, Cmd.none )


extractInt : String -> Maybe Int -> Maybe Int
extractInt string default =
    if String.isEmpty string then
        Nothing
    else
        String.toInt string
            |> Result.map Just
            |> Result.withDefault default

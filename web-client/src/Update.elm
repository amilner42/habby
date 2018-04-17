module Update exposing (..)

import Api
import Date
import DefaultServices.Infix exposing (..)
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

        getHabitsAndHabitDataAndFrequencyStats : Cmd Msg
        getHabitsAndHabitDataAndFrequencyStats =
            Api.queryHabitsAndHabitDataAndFrequencyStats
                model.ymd
                model.apiBaseUrl
                OnGetHabitsAndHabitDataAndFrequencyStatsFailure
                OnGetHabitsAndHabitDataAndFrequencyStatsSuccess

        getHistoryViewerFrequencyStats : YmdDate.YmdDate -> Cmd Msg
        getHistoryViewerFrequencyStats ymd =
            Api.queryPastFrequencyStats
                ymd
                model.apiBaseUrl
                OnGetPastFrequencyStatsFailure
                OnGetPastFrequencyStatsSuccess
    in
        case msg of
            NoOp ->
                ( model, Cmd.none )

            OnLocationChange location ->
                -- TODO
                ( model, Cmd.none )

            TickMinute time ->
                let
                    newYmd =
                        time |> Date.fromTime |> YmdDate.fromDate
                in
                    if model.ymd /= newYmd then
                        ( { model | ymd = newYmd }
                        , Api.queryHabitsAndHabitDataAndFrequencyStats
                            newYmd
                            model.apiBaseUrl
                            OnGetHabitsAndHabitDataAndFrequencyStatsFailure
                            OnGetHabitsAndHabitDataAndFrequencyStatsSuccess
                        )
                    else
                        ( model, Cmd.none )

            OnGetHabitsAndHabitDataAndFrequencyStatsFailure apiError ->
                ( { model
                    | allHabits = RemoteData.Failure apiError
                    , allHabitData = RemoteData.Failure apiError
                    , allFrequencyStats = RemoteData.Failure apiError
                  }
                , Cmd.none
                )

            OnGetHabitsAndHabitDataAndFrequencyStatsSuccess { habits, habitData, frequencyStatsList } ->
                ( { model
                    | allHabits = RemoteData.Success habits
                    , allHabitData = RemoteData.Success habitData
                    , allFrequencyStats = RemoteData.Success frequencyStatsList
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
                let
                    newModel =
                        { model
                            | allHabitData =
                                RemoteData.map
                                    (\allHabitData ->
                                        Util.replaceOrAdd allHabitData (.id >> (==) updatedHabitDatum.id) updatedHabitDatum
                                    )
                                    model.allHabitData
                            , editingTodayHabitAmount =
                                Dict.update updatedHabitDatum.habitId (always Nothing) model.editingTodayHabitAmount
                            , editingHistoryHabitAmount =
                                Dict.update
                                    (YmdDate.toSimpleString updatedHabitDatum.date)
                                    (Maybe.map (Dict.update updatedHabitDatum.habitId (always Nothing)))
                                    model.editingHistoryHabitAmount
                        }
                in
                    newModel
                        ! [ getHabitsAndHabitDataAndFrequencyStats
                          , (case newModel.historyViewerSelectedDate of
                                Just ymd ->
                                    getHistoryViewerFrequencyStats ymd

                                Nothing ->
                                    Cmd.none
                            )
                          ]

            OnToggleHistoryViewer ->
                ( { model | openHistoryViewer = not model.openHistoryViewer }
                , Cmd.none
                )

            OnToggleTodayViewer ->
                ( { model | openTodayViewer = not model.openTodayViewer }, Cmd.none )

            OnHistoryViewerDateInput newDateInput ->
                ( { model
                    | historyViewerDateInput =
                        newDateInput
                            |> String.filter
                                (\char -> List.member char [ '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '/' ])
                  }
                , Cmd.none
                )

            OnHistoryViewerSelectYesterday ->
                let
                    yesterday =
                        YmdDate.addDays -1 model.ymd
                in
                    update (SetHistoryViewerSelectedDate yesterday) model

            OnHistoryViewerSelectBeforeYesterday ->
                let
                    beforeYesterday =
                        YmdDate.addDays -2 model.ymd
                in
                    update (SetHistoryViewerSelectedDate beforeYesterday) model

            OnHistoryViewerSelectDateInput ->
                let
                    ymd =
                        YmdDate.fromSimpleString model.historyViewerDateInput
                in
                    (case ymd of
                        Just ymd ->
                            update (SetHistoryViewerSelectedDate ymd) model

                        Nothing ->
                            -- TODO: show error message because of invalid date input
                            ( model, Cmd.none )
                    )

            SetHistoryViewerSelectedDate ymd ->
                { model | historyViewerSelectedDate = Just ymd } ! [ getHistoryViewerFrequencyStats ymd ]

            OnGetPastFrequencyStatsFailure apiError ->
                ( { model | historyViewerFrequencyStats = RemoteData.Failure apiError }, Cmd.none )

            OnGetPastFrequencyStatsSuccess { frequencyStatsList } ->
                ( { model | historyViewerFrequencyStats = RemoteData.Success frequencyStatsList }, Cmd.none )

            OnHistoryViewerChangeDate ->
                ( { model | historyViewerSelectedDate = Nothing }, Cmd.none )

            OnHistoryViewerHabitDataInput forDate habitId newInput ->
                let
                    editingHabitDataDict =
                        model.editingHistoryHabitAmount
                            |> Dict.get (YmdDate.toSimpleString forDate)
                            ?> Dict.empty

                    newAmount =
                        extractInt newInput (Dict.get habitId editingHabitDataDict)

                    updatedEditingHabitDataDict =
                        editingHabitDataDict |> Dict.update habitId (always newAmount)
                in
                    ( { model
                        | editingHistoryHabitAmount =
                            model.editingHistoryHabitAmount
                                |> Dict.update
                                    (YmdDate.toSimpleString forDate)
                                    (always <| Just updatedEditingHabitDataDict)
                      }
                    , Cmd.none
                    )


extractInt : String -> Maybe Int -> Maybe Int
extractInt string default =
    if String.isEmpty string then
        Nothing
    else
        String.toInt string
            |> Result.map Just
            |> Result.withDefault default

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

        updateEditHabit updater =
            { model | editHabit = updater model.editHabit }

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

            OnHabitMouseEnter habitId ->
                ( { model | editHabitIconHabitID = Just habitId }, Cmd.none )

            OnHabitMouseLeave ->
                ( { model | editHabitIconHabitID = Nothing }, Cmd.none )

            OnEditHabitIconClick habit ->
                let
                    updateShowDialog eh =
                        { eh | showDialog = True }

                    { id, name, description, frequency, unitNameSingular, unitNamePlural } =
                        Habit.getCommonFields habit

                    updateCommonFields eh =
                        if eh.habitId /= Just id then
                            { eh
                                | habitId = Just id
                                , originalName = name
                                , name = name
                                , originalDescription = description ?> ""
                                , description = description ?> ""
                                , originalUnitNameSingular = unitNameSingular
                                , unitNameSingular = unitNameSingular
                                , originalUnitNamePlural = unitNamePlural
                                , unitNamePlural = unitNamePlural
                            }
                        else
                            eh

                    updateKindAndTimeOfDay eh =
                        case habit of
                            Habit.GoodHabit h ->
                                { eh
                                    | kind = Habit.GoodHabitKind
                                    , originalGoodHabitTime = h.timeOfDay
                                    , goodHabitTime = h.timeOfDay
                                }

                            Habit.BadHabit _ ->
                                { eh | kind = Habit.BadHabitKind }

                    updateFrequencyFields eh =
                        case frequency of
                            Habit.EveryXDayFrequency f ->
                                { eh
                                    | frequencyKind = Habit.EveryXDayFrequencyKind
                                    , originalFrequencyKind = Habit.EveryXDayFrequencyKind
                                    , times = Just f.times
                                    , originalTimes = Just f.times
                                    , days = Just f.days
                                    , originalDays = Just f.days
                                }

                            Habit.TotalWeekFrequency timesPerWeek ->
                                { eh
                                    | frequencyKind = Habit.TotalWeekFrequencyKind
                                    , originalFrequencyKind = Habit.TotalWeekFrequencyKind
                                    , timesPerWeek = Just timesPerWeek
                                    , originalTimesPerWeek = Just timesPerWeek
                                }

                            Habit.SpecificDayOfWeekFrequency f ->
                                { eh
                                    | frequencyKind = Habit.SpecificDayOfWeekFrequencyKind
                                    , originalFrequencyKind = Habit.SpecificDayOfWeekFrequencyKind
                                    , mondayTimes = Just f.monday
                                    , tuesdayTimes = Just f.tuesday
                                    , wednesdayTimes = Just f.wednesday
                                    , thursdayTimes = Just f.thursday
                                    , fridayTimes = Just f.friday
                                    , saturdayTimes = Just f.saturday
                                    , sundayTimes = Just f.sunday
                                }
                in
                    ( { model
                        | editHabit =
                            model.editHabit
                                |> updateShowDialog
                                |> updateCommonFields
                                |> updateKindAndTimeOfDay
                                |> updateFrequencyFields
                      }
                    , Cmd.none
                    )

            OnEditHabitRevertAllToDefaults ->
                ( updateEditHabit
                    (\eh ->
                        { eh
                            | kind = eh.originalKind
                            , name = eh.originalName
                            , description = eh.originalDescription
                            , goodHabitTime = eh.originalGoodHabitTime
                            , unitNameSingular = eh.originalUnitNameSingular
                            , unitNamePlural = eh.originalUnitNamePlural
                            , frequencyKind = eh.originalFrequencyKind
                            , timesPerWeek = eh.originalTimesPerWeek
                            , mondayTimes = eh.originalMondayTimes
                            , tuesdayTimes = eh.originalTuesdayTimes
                            , wednesdayTimes = eh.originalWednesdayTimes
                            , thursdayTimes = eh.originalThursdayTimes
                            , fridayTimes = eh.originalFridayTimes
                            , saturdayTimes = eh.originalSaturdayTimes
                            , sundayTimes = eh.originalSundayTimes
                            , times = eh.originalTimes
                            , days = eh.originalDays
                        }
                    )
                , Cmd.none
                )

            OnSelectEditHabitKind habitKind ->
                ( updateEditHabit (\eh -> { eh | kind = habitKind }), Cmd.none )

            OnEditHabitNameInput name ->
                ( updateEditHabit (\eh -> { eh | name = name }), Cmd.none )

            OnEditHabitDescriptionInput desc ->
                ( updateEditHabit (\eh -> { eh | description = desc }), Cmd.none )

            OnSelectEditHabitGoodHabitTime habitTime ->
                ( updateEditHabit (\eh -> { eh | goodHabitTime = habitTime }), Cmd.none )

            OnEditHabitUnitNameSingularInput uns ->
                ( updateEditHabit (\eh -> { eh | unitNameSingular = uns }), Cmd.none )

            OnEditHabitUnitNamePluralInput unp ->
                ( updateEditHabit (\eh -> { eh | unitNamePlural = unp }), Cmd.none )

            OnEditHabitSelectFrequencyKind fk ->
                ( updateEditHabit (\eh -> { eh | frequencyKind = fk }), Cmd.none )

            OnAbortEditHabitDialog ->
                ( updateEditHabit (\eh -> { eh | showDialog = False }), Cmd.none )

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

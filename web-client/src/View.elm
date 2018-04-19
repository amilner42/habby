module View exposing (..)

import DefaultServices.Infix exposing (..)
import DefaultServices.Util as Util
import Dialog
import Dict
import HabitUtil
import Html exposing (Html, button, div, hr, i, input, span, text, textarea, h3)
import Html.Attributes exposing (class, classList, placeholder, value, hidden)
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave)
import Keyboard.Extra as KK
import Maybe.Extra as Maybe
import Model exposing (Model)
import Models.ApiError as ApiError
import Models.FrequencyStats as FrequencyStats
import Models.Habit as Habit
import Models.HabitData as HabitData
import Models.YmdDate as YmdDate
import Msg exposing (Msg(..))
import RemoteData
import String.Extra


view : Model -> Html Msg
view model =
    div
        [ class "view" ]
        [ Util.bootstrap
        , renderTodayPanel
            model.ymd
            model.allHabits
            model.editHabitIconHabitID
            model.allHabitData
            model.allFrequencyStats
            model.addHabit
            model.editingTodayHabitAmount
            model.openTodayViewer
        , renderHistoryViewerPanel
            model.openHistoryViewer
            model.historyViewerDateInput
            model.historyViewerSelectedDate
            model.allHabits
            model.editHabitIconHabitID
            model.allHabitData
            model.historyViewerFrequencyStats
            model.editingHistoryHabitAmount
        , Dialog.view
            (if model.editHabit.showDialog then
                Just (editHabitDialogConfig model)
             else
                Nothing
            )
        ]


renderTodayPanel :
    YmdDate.YmdDate
    -> RemoteData.RemoteData ApiError.ApiError (List Habit.Habit)
    -> Maybe String
    -> RemoteData.RemoteData ApiError.ApiError (List HabitData.HabitData)
    -> RemoteData.RemoteData ApiError.ApiError (List FrequencyStats.FrequencyStats)
    -> Habit.AddHabitInputData
    -> Dict.Dict String Int
    -> Bool
    -> Html Msg
renderTodayPanel ymd rdHabits editHabitIconHabitID rdHabitData rdFrequencyStatsList addHabit editingHabitDataDict openView =
    let
        createHabitData =
            Habit.extractCreateHabit addHabit
    in
        div
            [ class "today-panel" ]
            [ div [ class "today-panel-title", onClick OnToggleTodayViewer ] [ text "Today's Progress" ]
            , dropdownIcon openView NoOp
            , div [ class "today-panel-date" ] [ text <| YmdDate.prettyPrint ymd ]
            , case ( rdHabits, rdHabitData ) of
                ( RemoteData.Success habits, RemoteData.Success habitData ) ->
                    let
                        ( goodHabits, badHabits ) =
                            Habit.splitHabits habits

                        ( sortedGoodHabits, sortedBadHabits ) =
                            case rdFrequencyStatsList of
                                RemoteData.Success frequencyStatsList ->
                                    ( HabitUtil.sortHabitsByCurrentFragment frequencyStatsList goodHabits
                                    , HabitUtil.sortHabitsByCurrentFragment frequencyStatsList badHabits
                                    )

                                _ ->
                                    ( goodHabits, badHabits )

                        renderHabit habit =
                            renderHabitBox
                                (case rdFrequencyStatsList of
                                    RemoteData.Success frequencyStatsList ->
                                        HabitUtil.findFrequencyStatsForHabit
                                            habit
                                            frequencyStatsList

                                    _ ->
                                        Err "Frequency stats not available for any habits"
                                )
                                ymd
                                habitData
                                editingHabitDataDict
                                OnHabitDataInput
                                SetHabitData
                                editHabitIconHabitID
                                habit
                    in
                        div [ classList [ ( "display-none", not openView ) ] ]
                            [ div
                                [ class "habit-list good-habits" ]
                                (List.map renderHabit sortedGoodHabits)
                            , div
                                [ class "habit-list bad-habits" ]
                                (List.map renderHabit sortedBadHabits)
                            , button
                                [ class "add-habit"
                                , onClick <|
                                    if addHabit.openView then
                                        OnCancelAddHabit
                                    else
                                        OnOpenAddHabit
                                ]
                                [ text <|
                                    if addHabit.openView then
                                        "Cancel"
                                    else
                                        "Add Habit"
                                ]
                            ]

                ( RemoteData.Failure apiError, _ ) ->
                    text "Failure..."

                ( _, RemoteData.Failure apiError ) ->
                    text "Failure..."

                _ ->
                    text "Loading..."
            , hr [ classList [ ( "add-habit-line-breaker", True ), ( "visibility-hidden height-0", not addHabit.openView ) ] ] []
            , div
                [ classList [ ( "add-habit-input-form", True ), ( "display-none", not addHabit.openView ) ] ]
                [ div
                    [ class "add-habit-input-form-habit-tag-name" ]
                    [ button
                        [ classList [ ( "selected", addHabit.kind == Habit.GoodHabitKind ) ]
                        , onClick <| OnSelectAddHabitKind Habit.GoodHabitKind
                        ]
                        [ text "Good Habit" ]
                    , button
                        [ classList [ ( "selected", addHabit.kind == Habit.BadHabitKind ) ]
                        , onClick <| OnSelectAddHabitKind Habit.BadHabitKind
                        ]
                        [ text "Bad Habit" ]
                    ]
                , div
                    [ class "add-habit-input-form-name-and-description" ]
                    [ input
                        [ class "add-habit-input-form-name"
                        , placeholder "Name..."
                        , onInput OnAddHabitNameInput
                        , value addHabit.name
                        ]
                        []
                    , textarea
                        [ class "add-habit-input-form-description"
                        , placeholder "Short description..."
                        , onInput OnAddHabitDescriptionInput
                        , value addHabit.description
                        ]
                        []
                    ]
                , div
                    [ classList
                        [ ( "add-habit-input-form-time-of-day", True )
                        , ( "display-none", addHabit.kind /= Habit.GoodHabitKind )
                        ]
                    ]
                    [ button
                        [ classList [ ( "habit-time-of-day", True ), ( "selected", addHabit.goodHabitTime == Habit.Anytime ) ]
                        , onClick <| OnSelectAddGoodHabitTime Habit.Anytime
                        ]
                        [ text "ANYTIME" ]
                    , button
                        [ classList [ ( "habit-time-of-day", True ), ( "selected", addHabit.goodHabitTime == Habit.Morning ) ]
                        , onClick <| OnSelectAddGoodHabitTime Habit.Morning
                        ]
                        [ text "MORNING" ]
                    , button
                        [ classList [ ( "habit-time-of-day", True ), ( "selected", addHabit.goodHabitTime == Habit.Evening ) ]
                        , onClick <| OnSelectAddGoodHabitTime Habit.Evening
                        ]
                        [ text "EVENING" ]
                    ]
                , div
                    [ class "add-habit-input-form-unit-name" ]
                    [ input
                        [ class "habit-unit-name-singular"
                        , placeholder "Unit name singular..."
                        , onInput OnAddHabitUnitNameSingularInput
                        , value addHabit.unitNameSingular
                        ]
                        []
                    , input
                        [ class "habit-unit-name-plural"
                        , placeholder "Unit name plural..."
                        , onInput OnAddHabitUnitNamePluralInput
                        , value addHabit.unitNamePlural
                        ]
                        []
                    ]
                , div
                    [ class "add-habit-input-form-frequency-tag-name" ]
                    [ button
                        [ classList [ ( "selected", addHabit.frequencyKind == Habit.TotalWeekFrequencyKind ) ]
                        , onClick <| OnAddHabitSelectFrequencyKind Habit.TotalWeekFrequencyKind
                        ]
                        [ text "X Per Week" ]
                    , button
                        [ classList [ ( "selected", addHabit.frequencyKind == Habit.SpecificDayOfWeekFrequencyKind ) ]
                        , onClick <| OnAddHabitSelectFrequencyKind Habit.SpecificDayOfWeekFrequencyKind
                        ]
                        [ text "Specific Days of Week" ]
                    , button
                        [ classList [ ( "selected", addHabit.frequencyKind == Habit.EveryXDaysFrequencyKind ) ]
                        , onClick <| OnAddHabitSelectFrequencyKind Habit.EveryXDaysFrequencyKind
                        ]
                        [ text "Y Per X Days" ]
                    ]
                , div
                    [ classList
                        [ ( "add-habit-input-form-x-times-per-week", True )
                        , ( "display-none", addHabit.frequencyKind /= Habit.TotalWeekFrequencyKind )
                        ]
                    ]
                    [ input
                        [ placeholder "X"
                        , onInput OnAddHabitTimesPerWeekInput
                        , value (addHabit.timesPerWeek ||> toString ?> "")
                        ]
                        []
                    ]
                , div
                    [ classList
                        [ ( "add-habit-input-form-specific-days-of-week", True )
                        , ( "display-none", addHabit.frequencyKind /= Habit.SpecificDayOfWeekFrequencyKind )
                        ]
                    ]
                    [ input
                        [ placeholder "Monday"
                        , onInput OnAddHabitSpecificDayMondayInput
                        , value (addHabit.mondayTimes ||> toString ?> "")
                        ]
                        []
                    , input
                        [ placeholder "Tuesday"
                        , onInput OnAddHabitSpecificDayTuesdayInput
                        , value (addHabit.tuesdayTimes ||> toString ?> "")
                        ]
                        []
                    , input
                        [ placeholder "Wednesday"
                        , onInput OnAddHabitSpecificDayWednesdayInput
                        , value (addHabit.wednesdayTimes ||> toString ?> "")
                        ]
                        []
                    , input
                        [ placeholder "Thursday"
                        , onInput OnAddHabitSpecificDayThursdayInput
                        , value (addHabit.thursdayTimes ||> toString ?> "")
                        ]
                        []
                    , input
                        [ placeholder "Friday"
                        , onInput OnAddHabitSpecificDayFridayInput
                        , value (addHabit.fridayTimes ||> toString ?> "")
                        ]
                        []
                    , input
                        [ placeholder "Saturday"
                        , onInput OnAddHabitSpecificDaySaturdayInput
                        , value (addHabit.saturdayTimes ||> toString ?> "")
                        ]
                        []
                    , input
                        [ placeholder "Sunday"
                        , onInput OnAddHabitSpecificDaySundayInput
                        , value (addHabit.sundayTimes ||> toString ?> "")
                        ]
                        []
                    ]
                , div
                    [ classList
                        [ ( "add-habit-input-form-x-times-per-y-days", True )
                        , ( "display-none", addHabit.frequencyKind /= Habit.EveryXDaysFrequencyKind )
                        ]
                    ]
                    [ input
                        [ placeholder "Times"
                        , onInput OnAddHabitTimesInput
                        , value (addHabit.times ||> toString ?> "")
                        ]
                        []
                    , input
                        [ placeholder "Days"
                        , onInput OnAddHabitDaysInput
                        , value (addHabit.days ||> toString ?> "")
                        ]
                        []
                    ]
                , case createHabitData of
                    Nothing ->
                        Util.hiddenDiv

                    Just createHabitData ->
                        button
                            [ class "add-new-habit"
                            , onClick <| AddHabit createHabitData
                            ]
                            [ text "Create Habit" ]
                ]
            ]


renderHistoryViewerPanel :
    Bool
    -> String
    -> Maybe YmdDate.YmdDate
    -> RemoteData.RemoteData ApiError.ApiError (List Habit.Habit)
    -> Maybe String
    -> RemoteData.RemoteData ApiError.ApiError (List HabitData.HabitData)
    -> RemoteData.RemoteData ApiError.ApiError (List FrequencyStats.FrequencyStats)
    -> Dict.Dict String (Dict.Dict String Int)
    -> Html Msg
renderHistoryViewerPanel openView dateInput selectedDate rdHabits editHabitIconHabitID rdHabitData rdFrequencyStatsList editingHabitDataDictDict =
    case ( rdHabits, rdHabitData ) of
        ( RemoteData.Success habits, RemoteData.Success habitData ) ->
            div
                [ class "history-viewer-panel" ]
                [ div [ class "history-viewer-panel-title", onClick OnToggleHistoryViewer ] [ text "Browse and Edit History" ]
                , dropdownIcon openView NoOp
                , if not openView then
                    Util.hiddenDiv
                  else
                    case selectedDate of
                        Nothing ->
                            div
                                [ classList [ ( "date-entry", True ), ( "display-none", not openView ) ] ]
                                [ span [ class "select-yesterday", onClick OnHistoryViewerSelectYesterday ] [ text "yesterday" ]
                                , span
                                    [ class "before-yesterday", onClick OnHistoryViewerSelectBeforeYesterday ]
                                    [ text "before yesterday" ]
                                , span [ class "separating-text" ] [ text "or exact date" ]
                                , input
                                    [ placeholder "dd/mm/yy"
                                    , onInput OnHistoryViewerDateInput
                                    , value dateInput
                                    , Util.onKeydown
                                        (\key ->
                                            if key == KK.Enter then
                                                Just OnHistoryViewerSelectDateInput
                                            else
                                                Nothing
                                        )
                                    ]
                                    []
                                ]

                        Just selectedDate ->
                            let
                                ( goodHabits, badHabits ) =
                                    Habit.splitHabits habits

                                ( sortedGoodHabits, sortedBadHabits ) =
                                    case rdFrequencyStatsList of
                                        RemoteData.Success frequencyStatsList ->
                                            ( HabitUtil.sortHabitsByCurrentFragment frequencyStatsList goodHabits
                                            , HabitUtil.sortHabitsByCurrentFragment frequencyStatsList badHabits
                                            )

                                        _ ->
                                            ( goodHabits, badHabits )

                                editingHabitDataDict =
                                    Dict.get (YmdDate.toSimpleString selectedDate) editingHabitDataDictDict
                                        ?> Dict.empty

                                renderHabit habit =
                                    renderHabitBox
                                        (case rdFrequencyStatsList of
                                            RemoteData.Success frequencyStatsList ->
                                                HabitUtil.findFrequencyStatsForHabit
                                                    habit
                                                    frequencyStatsList

                                            _ ->
                                                Err "Frequency stats not available for any habits"
                                        )
                                        selectedDate
                                        habitData
                                        editingHabitDataDict
                                        (OnHistoryViewerHabitDataInput selectedDate)
                                        SetHabitData
                                        editHabitIconHabitID
                                        habit
                            in
                                div
                                    []
                                    [ span [ class "selected-date-title" ] [ text <| YmdDate.prettyPrint selectedDate ]
                                    , span [ class "change-date", onClick OnHistoryViewerChangeDate ] [ text "change date" ]
                                    , div [ class "habit-list good-habits" ] <| List.map renderHabit sortedGoodHabits
                                    , div [ class "habit-list bad-habits" ] <| List.map renderHabit sortedBadHabits
                                    ]
                ]

        ( RemoteData.Failure apiError, _ ) ->
            text "Failure..."

        ( _, RemoteData.Failure apiError ) ->
            text "Failure..."

        _ ->
            text "Loading..."


dropdownIcon : Bool -> msg -> Html msg
dropdownIcon openView msg =
    i
        [ class "material-icons"
        , onClick msg
        ]
        [ text <|
            if openView then
                "arrow_drop_down"
            else
                "arrow_drop_up"
        ]


{-| Renders a habit box with the habit data loaded for that particular date.

Requires 2 event handlers, 1 for handling when data is input into the habit box and 1 for when the user wants to
update the habit data.

-}
renderHabitBox :
    Result String FrequencyStats.FrequencyStats
    -> YmdDate.YmdDate
    -> List HabitData.HabitData
    -> Dict.Dict String Int
    -> (String -> String -> Msg)
    -> (YmdDate.YmdDate -> String -> Maybe Int -> Msg)
    -> Maybe String
    -> Habit.Habit
    -> Html Msg
renderHabitBox habitStats ymd habitData editingHabitDataDict onHabitDataInput setHabitData editHabitIconHabitID habit =
    let
        habitRecord =
            Habit.getCommonFields habit

        habitDatum =
            List.filter (\{ habitId, date } -> habitId == habitRecord.id && date == ymd) habitData
                |> List.head
                |> (\habitDatum ->
                        case habitDatum of
                            Nothing ->
                                0

                            Just { amount } ->
                                amount
                   )

        editingHabitData =
            Dict.get habitRecord.id editingHabitDataDict

        showEditHabitIcon =
            False <? Maybe.map (\i -> i == habitRecord.id) editHabitIconHabitID

        isCurrentFragmentSuccessful =
            case habitStats of
                Err _ ->
                    False

                Ok stats ->
                    HabitUtil.isHabitCurrentFragmentSuccessful habit stats

        frequencyStatisticDiv str =
            div
                [ class "frequency-statistic" ]
                [ text str ]
    in
        div
            [ class "habit-box-and-edit-habit-form" ]
            [ div
                [ class
                    (if isCurrentFragmentSuccessful then
                        "habit-success"
                     else
                        "habit-failure"
                    )
                , onMouseEnter <| OnHabitMouseEnter habitRecord.id
                , onMouseLeave <| OnHabitMouseLeave
                ]
                [ div [ class "habit-name" ] [ text habitRecord.name ]
                , (case habitStats of
                    Err _ ->
                        frequencyStatisticDiv "Error retriving performance stats"

                    Ok stats ->
                        div [ class "frequency-stats-list" ]
                            [ div
                                [ class "current-progress" ]
                                [ text <|
                                    (toString stats.currentFragmentTotal)
                                        ++ " out of "
                                        ++ (toString stats.currentFragmentGoal)
                                        ++ " "
                                        ++ habitRecord.unitNamePlural
                                ]
                            , frequencyStatisticDiv ("Days left: " ++ (toString stats.currentFragmentDaysLeft))
                            , frequencyStatisticDiv
                                ((toString <|
                                    round <|
                                        (toFloat stats.successfulFragments)
                                            * 100
                                            / (toFloat stats.totalFragments)
                                 )
                                    ++ "%"
                                )
                            , frequencyStatisticDiv ("Streak: " ++ (toString stats.currentFragmentStreak))
                            , frequencyStatisticDiv ("Best streak: " ++ (toString stats.bestFragmentStreak))
                            , frequencyStatisticDiv ("Total done: " ++ (toString stats.totalDone))
                            ]
                  )
                , div
                    [ classList
                        [ ( "edit-habit-icon", True )
                        , ( "hidden", not showEditHabitIcon )
                        ]
                    , onClick <| OnEditHabitIconClick habit
                    ]
                    []
                , div
                    [ classList
                        [ ( "habit-amount-complete", True )
                        , ( "editing", Maybe.isJust <| editingHabitData )
                        ]
                    ]
                    [ input
                        [ placeholder <|
                            toString habitDatum
                                ++ " "
                                ++ (if habitDatum == 1 then
                                        habitRecord.unitNameSingular
                                    else
                                        habitRecord.unitNamePlural
                                   )
                        , onInput <| onHabitDataInput habitRecord.id
                        , Util.onKeydown
                            (\key ->
                                if key == KK.Enter then
                                    Just <| setHabitData ymd habitRecord.id editingHabitData
                                else
                                    Nothing
                            )
                        , value (editingHabitData ||> toString ?> "")
                        ]
                        []
                    , i
                        [ classList [ ( "material-icons", True ) ]
                        , onClick <| setHabitData ymd habitRecord.id editingHabitData
                        ]
                        [ text "check_box" ]
                    ]
                ]
            ]


editHabitDialogConfig : Model -> Dialog.Config Msg
editHabitDialogConfig model =
    let
        eh =
            model.editHabit
    in
        { closeMessage = Just OnAbortEditHabitDialog
        , containerClass = Nothing
        , header =
            Just <|
                div [ class "edit-habit-header" ]
                    [ h3
                        [ class "edit-habit-header" ]
                        [ text <| "Edit Habit: " ++ eh.originalName
                        ]
                    , button
                        [ class "revert-to-defaults-button"
                        , onClick OnEditHabitRevertAllToDefaults
                        ]
                        [ text "Revert all fields to default values" ]
                    ]
        , body =
            Just <|
                div
                    [ class "edit-habit-body" ]
                    [ div
                        [ class "select-habit-kind" ]
                        [ button
                            [ classList
                                [ ( "good-habit-button", True )
                                , ( "selected", eh.kind == Habit.GoodHabitKind )
                                ]
                            , onClick <| OnSelectEditHabitKind Habit.GoodHabitKind
                            ]
                            [ text "Good Habit" ]
                        , button
                            [ classList
                                [ ( "bad-habit-button", True )
                                , ( "selected", eh.kind == Habit.BadHabitKind )
                                ]
                            , onClick <| OnSelectEditHabitKind Habit.BadHabitKind
                            ]
                            [ text "Bad Habit" ]
                        ]
                    , div
                        []
                        [ text "Name: "
                        , input
                            [ class "edit-habit-text-input"
                            , placeholder eh.originalName
                            , onInput OnEditHabitNameInput
                            , value eh.name
                            ]
                            []
                        ]
                    , div
                        []
                        [ text "Description: "
                        , input
                            [ class "edit-habit-text-input"
                            , placeholder eh.originalDescription
                            , onInput OnEditHabitDescriptionInput
                            , value eh.description
                            ]
                            []
                        ]
                    , div
                        [ classList
                            [ ( "select-habit-time", True )
                            , ( "display-none", eh.kind /= Habit.GoodHabitKind )
                            ]
                        ]
                        [ button
                            [ classList
                                [ ( "habit-time-button", True )
                                , ( "selected", eh.goodHabitTime == Habit.Anytime )
                                ]
                            , onClick <| OnSelectEditHabitGoodHabitTime Habit.Anytime
                            ]
                            [ text "Anytime" ]
                        , button
                            [ classList
                                [ ( "habit-time-button", True )
                                , ( "selected", eh.goodHabitTime == Habit.Morning )
                                ]
                            , onClick <| OnSelectEditHabitGoodHabitTime Habit.Morning
                            ]
                            [ text "Morning" ]
                        , button
                            [ classList
                                [ ( "habit-time-button", True )
                                , ( "selected", eh.goodHabitTime == Habit.Evening )
                                ]
                            , onClick <| OnSelectEditHabitGoodHabitTime Habit.Evening
                            ]
                            [ text "Evening" ]
                        ]
                    , div
                        []
                        [ text "Unit name (singular): "
                        , input
                            [ class "edit-habit-text-input"
                            , placeholder eh.originalUnitNameSingular
                            , onInput OnEditHabitUnitNameSingularInput
                            , value eh.unitNameSingular
                            ]
                            []
                        ]
                    , div
                        []
                        [ text "Unit name (plural): "
                        , input
                            [ class "edit-habit-text-input"
                            , placeholder eh.originalUnitNamePlural
                            , onInput OnEditHabitUnitNamePluralInput
                            , value eh.unitNamePlural
                            ]
                            []
                        ]
                    , div
                        [ class "select-frequency-type" ]
                        [ button
                            [ classList
                                [ ( "frequency-type-button", True )
                                , ( "selected", eh.frequencyKind == Habit.TotalWeekFrequencyKind )
                                ]
                            , onClick <| OnEditHabitSelectFrequencyKind Habit.TotalWeekFrequencyKind
                            ]
                            [ text <|
                                ("X" <? toString <|| eh.timesPerWeek)
                                    ++ " "
                                    ++ (if (eh.timesPerWeek ?> 0) == 1 then
                                            String.Extra.toTitleCase eh.unitNameSingular
                                        else
                                            String.Extra.toTitleCase eh.unitNamePlural
                                       )
                                    ++ " Per Week"
                            ]
                        , button
                            [ classList
                                [ ( "frequency-type-button", True )
                                , ( "selected", eh.frequencyKind == Habit.SpecificDayOfWeekFrequencyKind )
                                ]
                            , onClick <| OnEditHabitSelectFrequencyKind Habit.SpecificDayOfWeekFrequencyKind
                            ]
                            [ text "Specific Days of Week" ]
                        , button
                            [ classList
                                [ ( "frequency-type-button", True )
                                , ( "selected", eh.frequencyKind == Habit.EveryXDaysFrequencyKind )
                                ]
                            , onClick <| OnEditHabitSelectFrequencyKind Habit.EveryXDaysFrequencyKind
                            ]
                            [ text <|
                                ("Y" <? toString <|| eh.times)
                                    ++ " "
                                    ++ (if (eh.times ?> 0) == 1 then
                                            String.Extra.toTitleCase eh.unitNameSingular
                                        else
                                            String.Extra.toTitleCase eh.unitNamePlural
                                       )
                                    ++ " Per "
                                    ++ (if (eh.days ?> 0) == 1 then
                                            "Day"
                                        else
                                            ("X" <? toString <|| eh.days) ++ " Days"
                                       )
                            ]
                        ]
                    , div
                        [ class "frequency-forms" ]
                        (let
                            titledInputFrequencyDiv : String -> Maybe Int -> (String -> Msg) -> Maybe Int -> Html Msg
                            titledInputFrequencyDiv title placeholderInt inputMsg valueInt =
                                div
                                    [ class "titled-input" ]
                                    [ text title
                                    , input
                                        [ class "frequency-input"
                                        , placeholder <| "" <? toString <|| placeholderInt
                                        , onInput inputMsg
                                        , value <| "" <? toString <|| valueInt
                                        ]
                                        []
                                    ]
                         in
                            [ div
                                [ classList
                                    [ ( "total-week-frequency-form", True )
                                    , ( "display-none", eh.frequencyKind /= Habit.TotalWeekFrequencyKind )
                                    ]
                                ]
                                [ titledInputFrequencyDiv
                                    ((String.Extra.toTitleCase eh.unitNamePlural) ++ " per week: ")
                                    eh.originalTimesPerWeek
                                    OnEditHabitTimesPerWeekInput
                                    eh.timesPerWeek
                                ]
                            , div
                                [ classList
                                    [ ( "specific-days-of-week-frequency-form", True )
                                    , ( "display-none", eh.frequencyKind /= Habit.SpecificDayOfWeekFrequencyKind )
                                    ]
                                ]
                                [ titledInputFrequencyDiv
                                    "Monday: "
                                    eh.originalMondayTimes
                                    OnEditHabitSpecificDayMondayInput
                                    eh.mondayTimes
                                , titledInputFrequencyDiv
                                    "Tuesday: "
                                    eh.originalTuesdayTimes
                                    OnEditHabitSpecificDayTuesdayInput
                                    eh.tuesdayTimes
                                , titledInputFrequencyDiv
                                    "Wednesday: "
                                    eh.originalWednesdayTimes
                                    OnEditHabitSpecificDayWednesdayInput
                                    eh.wednesdayTimes
                                , titledInputFrequencyDiv
                                    "Thursday: "
                                    eh.originalThursdayTimes
                                    OnEditHabitSpecificDayThursdayInput
                                    eh.thursdayTimes
                                , titledInputFrequencyDiv
                                    "Friday: "
                                    eh.originalFridayTimes
                                    OnEditHabitSpecificDayFridayInput
                                    eh.fridayTimes
                                , titledInputFrequencyDiv
                                    "Saturday: "
                                    eh.originalSaturdayTimes
                                    OnEditHabitSpecificDaySaturdayInput
                                    eh.saturdayTimes
                                , titledInputFrequencyDiv
                                    "Sunday: "
                                    eh.originalSundayTimes
                                    OnEditHabitSpecificDaySundayInput
                                    eh.sundayTimes
                                ]
                            , div
                                [ classList
                                    [ ( "every-x-days-frequency-form", True )
                                    , ( "display-none", eh.frequencyKind /= Habit.EveryXDaysFrequencyKind )
                                    ]
                                ]
                                [ titledInputFrequencyDiv
                                    (String.Extra.toTitleCase <| eh.unitNamePlural ++ ": ")
                                    eh.originalTimes
                                    OnEditHabitTimesInput
                                    eh.times
                                , titledInputFrequencyDiv
                                    "Days: "
                                    eh.originalDays
                                    OnEditHabitDaysInput
                                    eh.days
                                ]
                            ]
                        )
                    ]
        , footer =
            Just <|
                div [ class "edit-habit-footer" ]
                    [ button
                        [ classList
                            [ ( "edit-habit-submit-button", True )
                            , ( "btn", True )
                            , ( "btn-success", True )
                            ]
                        , onClick OnAbortEditHabitDialog
                        ]
                        [ text "OK" ]
                    , button
                        [ classList
                            [ ( "edit-habit-cancel-button", True )
                            , ( "btn", True )
                            , ( "btn-default", True )
                            ]
                        , onClick OnAbortEditHabitDialog
                        ]
                        [ text "cancel" ]
                    ]
        }

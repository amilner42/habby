module View exposing (..)

import DefaultServices.Infix exposing (..)
import DefaultServices.Util as Util
import Dict
import HabitUtil
import Html exposing (Html, button, div, hr, i, input, span, text, textarea)
import Html.Attributes exposing (class, classList, placeholder, value)
import Html.Events exposing (onClick, onInput)
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


view : Model -> Html Msg
view model =
    div
        [ class "view" ]
        [ renderTodayPanel
            model.ymd
            model.allHabits
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
            model.allHabitData
            model.allFrequencyStats
            model.editingHistoryHabitAmount
        ]


renderTodayPanel :
    YmdDate.YmdDate
    -> RemoteData.RemoteData ApiError.ApiError (List Habit.Habit)
    -> RemoteData.RemoteData ApiError.ApiError (List HabitData.HabitData)
    -> RemoteData.RemoteData ApiError.ApiError (List FrequencyStats.FrequencyStats)
    -> Habit.AddHabitInputData
    -> Dict.Dict String Int
    -> Bool
    -> Html Msg
renderTodayPanel ymd rdHabits rdHabitData rdFrequencyStatsList addHabit editingHabitDataDict openView =
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
                                            (.id (Habit.getCommonFields habit))
                                            frequencyStatsList

                                    _ ->
                                        Err "Frequency stats not available for any habits"
                                )
                                ymd
                                habitData
                                editingHabitDataDict
                                OnHabitDataInput
                                SetHabitData
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
                        [ classList [ ( "selected", addHabit.frequencyKind == Habit.EveryXDayFrequencyKind ) ]
                        , onClick <| OnAddHabitSelectFrequencyKind Habit.EveryXDayFrequencyKind
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
                        , ( "display-none", addHabit.frequencyKind /= Habit.EveryXDayFrequencyKind )
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
    -> RemoteData.RemoteData ApiError.ApiError (List HabitData.HabitData)
    -> RemoteData.RemoteData ApiError.ApiError (List FrequencyStats.FrequencyStats)
    -> Dict.Dict String (Dict.Dict String Int)
    -> Html Msg
renderHistoryViewerPanel openView dateInput selectedDate rdHabits rdHabitData rdFrequencyStatsList editingHabitDataDictDict =
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

                                editingHabitDataDict =
                                    Dict.get (YmdDate.toSimpleString selectedDate) editingHabitDataDictDict
                                        ?> Dict.empty

                                renderHabit habit =
                                    renderHabitBox
                                        -- (case rdFrequencyStatsList of
                                        --     RemoteData.Success frequencyStatsList ->
                                        --         HabitUtil.findFrequencyStatsForHabit
                                        --             (.id (Habit.getCommonFields habit))
                                        --             frequencyStatsList
                                        --
                                        --     _ ->
                                        --         Err "Frequency stats not available for any habits"
                                        -- )
                                        (Err "Ignore frequency stats for historical data")
                                        selectedDate
                                        habitData
                                        editingHabitDataDict
                                        (OnHistoryViewerHabitDataInput selectedDate)
                                        SetHabitData
                                        habit
                            in
                                div
                                    []
                                    [ span [ class "selected-date-title" ] [ text <| YmdDate.prettyPrint selectedDate ]
                                    , span [ class "change-date", onClick OnHistoryViewerChangeDate ] [ text "change date" ]
                                    , div [ class "habit-list good-habits" ] <| List.map renderHabit goodHabits
                                    , div [ class "habit-list bad-habits" ] <| List.map renderHabit badHabits
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
    -> Habit.Habit
    -> Html Msg
renderHabitBox habitStats ymd habitData editingHabitDataDict onHabitDataInput setHabitData habit =
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
    in
        div
            [ class "habit" ]
            [ div [ class "habit-name" ] [ text habitRecord.name ]
            , (case habitStats of
                Err _ ->
                    div [ class "frequency-stats" ] [ text "Frequency stats not available" ]

                Ok stats ->
                    div [ class "frequency-stats" ]
                        [ div []
                            [ text <|
                                "Days left: "
                                    ++ (toString stats.currentFragmentDaysLeft)
                            ]
                        , div
                            (case (compare stats.currentFragmentTotal stats.currentFragmentGoal) of
                                EQ ->
                                    [ class "current-fragment-success" ]

                                LT ->
                                    case habit of
                                        Habit.GoodHabit _ ->
                                            []

                                        Habit.BadHabit _ ->
                                            [ class "current-fragment-success" ]

                                GT ->
                                    case habit of
                                        Habit.GoodHabit _ ->
                                            [ class "current-fragment-success" ]

                                        Habit.BadHabit _ ->
                                            [ class "current-fragment-failure" ]
                            )
                            [ text <|
                                "Done: "
                                    ++ (toString stats.currentFragmentTotal)
                                    ++ "/"
                                    ++ (toString stats.currentFragmentGoal)
                            ]
                        ]
              )
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

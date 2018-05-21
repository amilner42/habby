module HabitUtil exposing (..)

{-| Module for useful Habit operations
-}

import Models.FrequencyStats as FrequencyStats
import Models.Habit as Habit


isHabitCurrentFragmentSuccessful : Habit.Habit -> FrequencyStats.FrequencyStats -> Bool
isHabitCurrentFragmentSuccessful habit frequencyStats =
    case habit of
        Habit.GoodHabit _ ->
            frequencyStats.currentFragmentTotal >= frequencyStats.currentFragmentGoal

        Habit.BadHabit _ ->
            frequencyStats.currentFragmentTotal <= frequencyStats.currentFragmentGoal


findFrequencyStatsForHabit : Habit.Habit -> List FrequencyStats.FrequencyStats -> Maybe FrequencyStats.FrequencyStats
findFrequencyStatsForHabit habit frequencyStats =
    List.filter (\stats -> stats.habitId == (habit |> Habit.getCommonFields |> .id)) frequencyStats
        |> List.head


type alias HabitStatsPair =
    ( Habit.Habit, FrequencyStats.FrequencyStats )


{-| For comparing 2 instances of `t`.
-}
type alias Comparator t =
    t -> t -> Order


{-| To be able to sort by multiple comparators (for breaking ties).
Inspired by <https://github.com/amilner42/code-tidbit/blob/a8a8b3b169eb7a4c929788095ae43353d6559752/frontend/src/DefaultServices/Sort.elm#L24>
-}
compareByAll : List (Comparator t) -> Comparator t
compareByAll comparators tOne tTwo =
    case comparators of
        [] ->
            EQ

        comparator :: restOfComparators ->
            case comparator tOne tTwo of
                LT ->
                    LT

                GT ->
                    GT

                EQ ->
                    compareByAll restOfComparators tOne tTwo


{-| Compares two habits by whether or not their current fragment goals have already been achieved. Prioritizes incomplete habits.
-}
compareHabitsByCompletion : Comparator HabitStatsPair
compareHabitsByCompletion ( habitOne, statsOne ) ( habitTwo, statsTwo ) =
    let
        isHabitOneGoalCompleted =
            isHabitCurrentFragmentSuccessful habitOne statsOne

        isHabitTwoGoalCompleted =
            isHabitCurrentFragmentSuccessful habitTwo statsTwo
    in
    if isHabitOneGoalCompleted == isHabitTwoGoalCompleted then
        EQ
    else if isHabitOneGoalCompleted then
        -- habit one is already complete, habit two is not
        GT
    else
        -- habit two is already complete, habit one is not
        LT


{-| Compares habits by urgency (days left in the current fragment). Prioritizes urgent habits.
-}
compareHabitsByDaysLeft : Comparator HabitStatsPair
compareHabitsByDaysLeft ( habitOne, statsOne ) ( habitTwo, statsTwo ) =
    compare statsOne.currentFragmentDaysLeft statsTwo.currentFragmentDaysLeft


{-| Compares habits by progress proportional to the current fragment goal. Prioritizes further-behind habits.
-}
compareHabitsByCurrentGoalProgress : Comparator HabitStatsPair
compareHabitsByCurrentGoalProgress ( habitOne, statsOne ) ( habitTwo, statsTwo ) =
    let
        -- Progress proportional to goal, e.g. 0.3 if the user has done 30% of the goal
        getCurrentGoalProgressFraction : FrequencyStats.FrequencyStats -> Float
        getCurrentGoalProgressFraction stats =
            toFloat stats.currentFragmentTotal
                / toFloat stats.currentFragmentGoal

        ( habitOneGoalProgressFraction, habitTwoGoalProgressFraction ) =
            ( getCurrentGoalProgressFraction statsOne, getCurrentGoalProgressFraction statsTwo )
    in
    case ( habitOne, habitTwo ) of
        ( Habit.GoodHabit _, Habit.GoodHabit _ ) ->
            -- The higher the fraction, the better the user is doing
            compare habitOneGoalProgressFraction habitTwoGoalProgressFraction

        ( Habit.BadHabit _, Habit.BadHabit _ ) ->
            -- The higher the fraction, the worse the user is doing
            compare habitTwoGoalProgressFraction habitOneGoalProgressFraction

        ( Habit.GoodHabit _, _ ) ->
            -- We probably shouldn't be sorting good habits and bad habits together,
            -- but if we are, we should display good habits first.
            LT

        _ ->
            GT


{-| Compares habits by how much of the current goal remains to be done. Prioritizes further-behind habits.
-}
compareHabitsByCurrentGoalRemaining : Comparator HabitStatsPair
compareHabitsByCurrentGoalRemaining ( habitOne, statsOne ) ( habitTwo, statsTwo ) =
    let
        getCurrentGoalRemaining stats =
            stats.currentFragmentGoal - stats.currentFragmentTotal

        ( habitOneGoalRemaining, habitTwoGoalRemaining ) =
            ( getCurrentGoalRemaining statsOne, getCurrentGoalRemaining statsTwo )
    in
    case ( habitOne, habitTwo ) of
        ( Habit.GoodHabit _, Habit.GoodHabit _ ) ->
            -- The more there is left to do for habit one, the worse it is doing
            compare habitTwoGoalRemaining habitOneGoalRemaining

        ( Habit.BadHabit _, Habit.BadHabit _ ) ->
            -- The bigger the difference between the goal and the total, the better
            -- the user is doing, the further down the list we should display the habit
            compare habitOneGoalRemaining habitTwoGoalRemaining

        ( Habit.GoodHabit _, _ ) ->
            -- We probably shouldn't be sorting good habits and bad habits together,
            -- but if we are, we should display good habits first.
            LT

        _ ->
            GT


{-| Returns the habits sorted by their progress in the current fragment.
Sorts first by completion, then by urgency, then by progress proportional to the current goal,
and then by how much (absolute, not proportional) of the current goal remains to be done.
-}
sortHabitsByCurrentFragment : List FrequencyStats.FrequencyStats -> List Habit.Habit -> List Habit.Habit
sortHabitsByCurrentFragment frequencyStatsList habits =
    let
        compareHabitsByCurrentFragment : Comparator Habit.Habit
        compareHabitsByCurrentFragment habitOne habitTwo =
            let
                habitOneFrequencyStats =
                    findFrequencyStatsForHabit habitOne frequencyStatsList

                habitTwoFrequencyStats =
                    findFrequencyStatsForHabit habitTwo frequencyStatsList
            in
            case ( habitOneFrequencyStats, habitTwoFrequencyStats ) of
                ( Nothing, Nothing ) ->
                    EQ

                ( Nothing, Just _ ) ->
                    GT

                ( Just _, Nothing ) ->
                    LT

                ( Just statsOne, Just statsTwo ) ->
                    compareByAll
                        [ compareHabitsByCompletion
                        , compareHabitsByDaysLeft
                        , compareHabitsByCurrentGoalProgress
                        , compareHabitsByCurrentGoalRemaining
                        ]
                        ( habitOne, statsOne )
                        ( habitTwo, statsTwo )
    in
    List.sortWith compareHabitsByCurrentFragment habits

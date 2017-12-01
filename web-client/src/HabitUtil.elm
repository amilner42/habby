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


findFrequencyStatsForHabit : Habit.Habit -> List FrequencyStats.FrequencyStats -> Result String FrequencyStats.FrequencyStats
findFrequencyStatsForHabit habit frequencyStats =
    let
        habitId =
            .id (Habit.getCommonFields habit)

        filteredFrequencyStatsByHabitId =
            List.filter (\stats -> stats.habitId == habitId) frequencyStats
    in
    case filteredFrequencyStatsByHabitId of
        [] ->
            Err ("No frequency stats found for habit " ++ habitId)

        [ s ] ->
            Ok s

        s1 :: s2 :: _ ->
            Err ("More than one frequency stats found for habit " ++ habitId ++ ", something fishy is going on")


{-| Returns the habits sorted by completion, urgency, and current goal progress.
-}
sortHabitsByCurrentFragment : List FrequencyStats.FrequencyStats -> List Habit.Habit -> List Habit.Habit
sortHabitsByCurrentFragment frequencyStatsList habits =
    let
        -- Compare habits by completion: show incomplete habits first
        compareHabitsByCompletion :
            Habit.Habit
            -> Habit.Habit
            -> FrequencyStats.FrequencyStats
            -> FrequencyStats.FrequencyStats
            -> Order
        compareHabitsByCompletion habitOne habitTwo statsOne statsTwo =
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

        --Compare habits by urgency: show urgent habits first
        compareHabitsByDaysLeft : FrequencyStats.FrequencyStats -> FrequencyStats.FrequencyStats -> Order
        compareHabitsByDaysLeft statsOne statsTwo =
            compare statsOne.currentFragmentDaysLeft statsTwo.currentFragmentDaysLeft

        --Compare habits by current goal progress: Show habits that the user is further behind on first
        compareHabitsByCurrentGoalProgress :
            Habit.Habit
            -> Habit.Habit
            -> FrequencyStats.FrequencyStats
            -> FrequencyStats.FrequencyStats
            -> Order
        compareHabitsByCurrentGoalProgress habitOne habitTwo statsOne statsTwo =
            let
                -- Progress relative to goal, e.g. 0.3 if the user has done 30% of the goal
                getCurrentGoalProgressFraction : FrequencyStats.FrequencyStats -> Float
                getCurrentGoalProgressFraction stats =
                    toFloat stats.currentFragmentTotal
                        / toFloat stats.currentFragmentGoal

                ( habitOneGoalProgressFraction, habitTwoGoalProgressFraction ) =
                    ( getCurrentGoalProgressFraction statsOne, getCurrentGoalProgressFraction statsTwo )
            in
            case ( habitOne, habitTwo ) of
                ( Habit.GoodHabit _, Habit.GoodHabit _ ) ->
                    -- The more progress the user has made, the less behind they are on
                    -- the habit, so display it further down
                    compare habitOneGoalProgressFraction habitTwoGoalProgressFraction

                ( Habit.BadHabit _, Habit.BadHabit _ ) ->
                    -- The more poorly the user has done, the more they should see it prominently
                    -- displayed so they can be aware and work on it
                    compare habitTwoGoalProgressFraction habitOneGoalProgressFraction

                ( Habit.GoodHabit _, _ ) ->
                    -- We probably shouldn't be sorting good habits and bad habits together,
                    -- but if we are, we should display good habits first.
                    LT

                _ ->
                    GT

        -- Compare habits by current goal remaining: Show habits that the user has more to do of first
        compareHabitsByCurrentGoalRemaining :
            Habit.Habit
            -> Habit.Habit
            -> FrequencyStats.FrequencyStats
            -> FrequencyStats.FrequencyStats
            -> Order
        compareHabitsByCurrentGoalRemaining habitOne habitTwo statsOne statsTwo =
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

        compareHabits : Habit.Habit -> Habit.Habit -> Order
        compareHabits habitOne habitTwo =
            let
                habitOneFrequencyStats =
                    findFrequencyStatsForHabit habitOne frequencyStatsList

                habitTwoFrequencyStats =
                    findFrequencyStatsForHabit habitTwo frequencyStatsList
            in
            case ( habitOneFrequencyStats, habitTwoFrequencyStats ) of
                ( Err _, Err _ ) ->
                    EQ

                ( Err _, Ok _ ) ->
                    GT

                ( Ok _, Err _ ) ->
                    LT

                ( Ok statsOne, Ok statsTwo ) ->
                    let
                        completionComparison =
                            compareHabitsByCompletion habitOne habitTwo statsOne statsTwo
                    in
                    if completionComparison == EQ then
                        let
                            daysLeftComparison =
                                compareHabitsByDaysLeft statsOne statsTwo
                        in
                        if daysLeftComparison == EQ then
                            let
                                currentGoalProgressComparison =
                                    compareHabitsByCurrentGoalProgress habitOne habitTwo statsOne statsTwo
                            in
                            if currentGoalProgressComparison == EQ then
                                compareHabitsByCurrentGoalRemaining habitOne habitTwo statsOne statsTwo
                            else
                                currentGoalProgressComparison
                        else
                            daysLeftComparison
                    else
                        completionComparison
    in
    List.sortWith compareHabits habits

module HabitUtil exposing (..)

{-| Module for useful Habit operations
-}

import Models.Habit as Habit
import Models.FrequencyStats as FrequencyStats


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
            (.id (Habit.getCommonFields habit))

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


{-| Returns the habits sorted first by whether the current fragment goal has already been achieved
(unfinished habits come first), and then by the number of days remaining in the current time fragment
(more urgent habit goals come first).
-}
sortHabitsByCurrentFragment : List FrequencyStats.FrequencyStats -> List Habit.Habit -> List Habit.Habit
sortHabitsByCurrentFragment frequencyStatsList habits =
    let
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
                            isHabitOneAlreadySuccessful =
                                isHabitCurrentFragmentSuccessful habitOne statsOne

                            isHabitTwoAlreadySuccessful =
                                isHabitCurrentFragmentSuccessful habitTwo statsTwo
                        in
                            if isHabitOneAlreadySuccessful == isHabitTwoAlreadySuccessful then
                                let
                                    daysLeftComparison =
                                        compare statsOne.currentFragmentDaysLeft statsTwo.currentFragmentDaysLeft
                                in
                                    case daysLeftComparison of
                                        EQ ->
                                            -- further sort by current fragment progress
                                            let
                                                getCurrentProgress : FrequencyStats.FrequencyStats -> Float
                                                getCurrentProgress stats =
                                                    ((toFloat stats.currentFragmentTotal)
                                                        / (toFloat stats.currentFragmentGoal)
                                                    )

                                                ( habitOneProgress, habitTwoProgress ) =
                                                    ( getCurrentProgress statsOne, getCurrentProgress statsTwo )
                                            in
                                                case ( habitOne, habitTwo ) of
                                                    ( Habit.GoodHabit _, Habit.GoodHabit _ ) ->
                                                        -- The more progress the user has made, the less behind they are on
                                                        -- the habit, so display it further down
                                                        compare habitOneProgress habitTwoProgress

                                                    ( Habit.BadHabit _, Habit.BadHabit _ ) ->
                                                        -- The more poorly the user has done, the more they should see it prominently
                                                        -- displayed so they can be aware and work on it
                                                        compare habitTwoProgress habitOneProgress

                                                    ( Habit.GoodHabit _, Habit.BadHabit _ ) ->
                                                        -- We probably shouldn't be sorting good habits and bad habits together,
                                                        -- but if we are, we should display good habits first.
                                                        LT

                                                    _ ->
                                                        GT

                                        _ ->
                                            daysLeftComparison
                            else if isHabitOneAlreadySuccessful then
                                -- We know they are different so if `isHabitOneAlreadySuccessful` is true
                                -- then `isHabitTwoAlreadySuccessful` must be false
                                GT
                            else
                                LT
    in
        List.sortWith compareHabits habits

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
                                compare statsOne.currentFragmentDaysLeft statsTwo.currentFragmentDaysLeft
                            else if isHabitOneAlreadySuccessful then
                                -- We know they are different so if `isHabitOneAlreadySuccessful` is true
                                -- then `isHabitTwoAlreadySuccessful` must be false
                                GT
                            else
                                LT
    in
        List.sortWith compareHabits habits

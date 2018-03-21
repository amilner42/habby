module HabitUtil exposing (..)

{-| Module for useful Habit operations
-}

import Models.Habit as Habit
import Models.FrequencyStats as FrequencyStats


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
                findHabitCurrentFragmentDaysLeft : Habit.Habit -> Maybe Int
                findHabitCurrentFragmentDaysLeft habit =
                    let
                        habitFrequencyStats =
                            findFrequencyStatsForHabit habit frequencyStatsList
                    in
                        case habitFrequencyStats of
                            Err err ->
                                Nothing

                            Ok stats ->
                                Just (.currentFragmentDaysLeft stats)

                habitOneCurrentFragmentDaysLeft =
                    findHabitCurrentFragmentDaysLeft habitOne

                habitTwoCurrentFragmentDaysLeft =
                    findHabitCurrentFragmentDaysLeft habitTwo
            in
                case ( habitOneCurrentFragmentDaysLeft, habitTwoCurrentFragmentDaysLeft ) of
                    ( Nothing, Nothing ) ->
                        EQ

                    ( Nothing, Just _ ) ->
                        GT

                    ( Just _, Nothing ) ->
                        LT

                    ( Just a, Just b ) ->
                        if a < b then
                            LT
                        else if a == b then
                            EQ
                        else
                            GT
    in
        List.sortWith compareHabits habits

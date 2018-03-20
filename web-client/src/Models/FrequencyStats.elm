module Models.FrequencyStats exposing (..)

import Json.Decode as Decode exposing (null)
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)


type alias FrequencyStats =
    { habitId : String
    , totalFragments : Int
    , successfulFragments : Int
    , totalDone : Int
    , currentFragmentStreak : Int
    , bestFragmentStreak : Int
    , currentFragmentTotal : Int
    , currentFragmentGoal : Int
    , currentFragmentDaysLeft : Int
    }


decodeFrequencyStats : Decode.Decoder FrequencyStats
decodeFrequencyStats =
    decode FrequencyStats
        |> required "habit_id" Decode.string
        |> required "total_fragments" Decode.int
        |> required "successful_fragments" Decode.int
        |> required "total_done" Decode.int
        |> required "current_fragment_streak" Decode.int
        |> required "best_fragment_streak" Decode.int
        |> required "current_fragment_total" Decode.int
        |> required "current_fragment_goal" Decode.int
        |> required "current_fragment_days_left" Decode.int


findFrequencyStatsByHabitId : String -> List FrequencyStats -> Result String FrequencyStats
findFrequencyStatsByHabitId habitId frequencyStats =
    let
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

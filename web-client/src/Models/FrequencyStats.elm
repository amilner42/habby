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
    , habitHasStarted : Bool
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
        |> required "habit_has_started" Decode.bool

module Api exposing (..)

import DefaultServices.Http exposing (post)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)
import Json.Encode as Encode
import Models.ApiError exposing (ApiError)
import Models.Habit as Habit
import Models.HabitData as HabitData


{-| Send the `query` to the graphql endpoint.
-}
graphQLRequest : String -> Decode.Decoder a -> String -> (ApiError -> b) -> (a -> b) -> Cmd b
graphQLRequest query decoder url handleError handleSuccess =
    post url decoder (Encode.object [ ( "query", Encode.string query ) ]) handleError handleSuccess


type alias HabitsAndHabitData =
    { habits : List Habit.Habit, habitData : List HabitData.HabitData }


{-| Query for all fields on all habits and habit data.
-}
queryHabitsAndHabitData : String -> (ApiError -> b) -> (HabitsAndHabitData -> b) -> Cmd b
queryHabitsAndHabitData =
    let
        habitAndHabitDataQueryString =
            """{
        \t\thabitData: get_habit_data {
        \t\t\t\t_id,
        \t\t\tamount,
        \t\t\tdate {
        \t\t\t\tday,
        \t\t\t\tmonth,
        \t\t\t\tyear
        \t\t\t},
        \t\t\thabit_id
        \t\t},
        \t
        \t\thabits: get_habits {
        \t\t\t__typename
        \t\t\t...on good_habit {
        \t\t\t\t_id,
        \t\t\t\tdescription,
        \t\t\t\tname,
        \t\t\t\tsuspended,
        \t\t\t\ttarget_frequency {
        \t\t\t\t\t__typename
        \t\t\t\t\t... on every_x_days_frequency {
        \t\t\t\t\t\tdays,
        \t\t\t\t\t\ttimes
        \t\t\t\t\t}
        \t\t\t\t\t...on total_week_frequency {
        \t\t\t\t\t\tweek
        \t\t\t\t\t}
        \t\t\t\t\t...on specific_day_of_week_frequency {
        \t\t\t\t\t\tfriday,
        \t\t\t\t\t\tmonday,
        \t\t\t\t\t\tsaturday,
        \t\t\t\t\t\tsunday,
        \t\t\t\t\t\tthursday,
        \t\t\t\t\t\ttuesday,
        \t\t\t\t\t\twednesday
        \t\t\t\t\t}
        \t\t\t\t}
        \t\t\t\ttime_of_day
        \t\t\t}
        \t\t\t...on bad_habit {
        \t\t\t\t_id,
        \t\t\t\tdescription,
        \t\t\t\tname,
        \t\t\t\tsuspended,
        \t\t\t\tthreshold_frequency {
        \t\t\t\t\t\t__typename
        \t\t\t\t\t... on every_x_days_frequency {
        \t\t\t\t\t\tdays,
        \t\t\t\t\t\ttimes
        \t\t\t\t\t}
        \t\t\t\t\t...on total_week_frequency {
        \t\t\t\t\t\tweek
        \t\t\t\t\t}
        \t\t\t\t\t...on specific_day_of_week_frequency {
        \t\t\t\t\t\tfriday,
        \t\t\t\t\t\tmonday,
        \t\t\t\t\t\tsaturday,
        \t\t\t\t\t\tsunday,
        \t\t\t\t\t\tthursday,
        \t\t\t\t\t\ttuesday,
        \t\t\t\t\t\twednesday
        \t\t\t\t\t}
        \t\t\t\t}\t\t
        \t\t\t}
        \t\t}
        }"""
    in
    graphQLRequest
        habitAndHabitDataQueryString
        (decode HabitsAndHabitData
            |> required "habits" (Decode.list Habit.decodeHabit)
            |> required "habitData" (Decode.list HabitData.decodeHabitData)
            |> Decode.at [ "data" ]
        )

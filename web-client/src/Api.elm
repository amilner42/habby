module Api exposing (..)

import DefaultServices.Http exposing (post)
import DefaultServices.Util as Util
import Dict
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)
import Json.Encode as Encode
import Models.ApiError exposing (ApiError)
import Models.Habit as Habit
import Models.HabitData as HabitData
import Models.FrequencyStats as FrequencyStats
import Models.YmdDate as YmdDate


{-| Send the `query` to the graphql endpoint.
-}
graphQLRequest : String -> Decode.Decoder a -> String -> (ApiError -> b) -> (a -> b) -> Cmd b
graphQLRequest query decoder url handleError handleSuccess =
    post url decoder (Encode.object [ ( "query", Encode.string query ) ]) handleError handleSuccess


type alias HabitsAndHabitDataAndFrequencyStats =
    { habits : List Habit.Habit
    , habitData : List HabitData.HabitData
    , frequencyStats : List FrequencyStats.FrequencyStats
    }


{-| Query for all fields on all habits and habit data.
-}
queryHabitsAndHabitDataAndFrequencyStats :
    YmdDate.YmdDate
    -> String
    -> (ApiError -> b)
    -> (HabitsAndHabitDataAndFrequencyStats -> b)
    -> Cmd b
queryHabitsAndHabitDataAndFrequencyStats ymd url handleError handleSuccess =
    let
        queryString =
            ("""{
  habits: get_habits {
    __typename
    ... on good_habit {
      _id
      description
      name
      suspended
      unit_name_singular
      unit_name_plural
      target_frequency {
        __typename
        ... on every_x_days_frequency {
          days
          times
        }
        ... on total_week_frequency {
          week
        }
        ... on specific_day_of_week_frequency {
          monday
          tuesday
          wednesday
          thursday
          friday
          saturday
          sunday
        }
      }
      time_of_day
    }
    ... on bad_habit {
      _id
      description
      name
      suspended
      unit_name_singular
      unit_name_plural
      threshold_frequency {
        __typename
        ... on every_x_days_frequency {
          days
          times
        }
        ... on total_week_frequency {
          week
        }
        ... on specific_day_of_week_frequency {
          monday
          tuesday
          wednesday
          thursday
          friday
          saturday
          sunday
        }
      }
    }
  }
  habitData: get_habit_data {
    _id
    amount
    date {
      day
      month
      year
    }
    habit_id
  }
  frequencyStats: get_frequency_stats(current_client_date: {year: """
                ++ (toString ymd.year)
                ++ ", month: "
                ++ (toString ymd.month)
                ++ ", day: "
                ++ (toString ymd.day)
                ++ """}) {
    habit_id
    total_fragments
    successful_fragments
    total_done
    current_fragment_streak
    best_fragment_streak
    current_fragment_total
    current_fragment_goal
    current_fragment_days_left
  }
}"""
            )
    in
        graphQLRequest
            queryString
            (decode HabitsAndHabitDataAndFrequencyStats
                |> required "habits" (Decode.list Habit.decodeHabit)
                |> required "habitData" (Decode.list HabitData.decodeHabitData)
                |> required "frequencyStats" (Decode.list FrequencyStats.decodeFrequencyStats)
                |> Decode.at [ "data" ]
            )
            url
            handleError
            handleSuccess


mutationAddHabit : Habit.CreateHabit -> String -> (ApiError -> b) -> (Habit.Habit -> b) -> Cmd b
mutationAddHabit createHabit =
    let
        commonFields =
            Habit.getCommonCreateFields createHabit

        templateDict =
            Dict.fromList
                [ ( "type_name"
                  , if isGoodHabit then
                        "good_habit"
                    else
                        "bad_habit"
                  )
                , ( "name", commonFields.name )
                , ( "description", commonFields.description )
                , ( "time_of_day"
                  , case createHabit of
                        Habit.CreateGoodHabit { timeOfDay } ->
                            "time_of_day: " ++ (toString timeOfDay |> String.toUpper) ++ ","

                        _ ->
                            ""
                  )
                , ( "unit_name_singular", commonFields.unitNameSingular )
                , ( "unit_name_plural", commonFields.unitNamePlural )
                , ( "frequency_name"
                  , if isGoodHabit then
                        "target_frequency"
                    else
                        "threshold_frequency"
                  )
                , ( "frequency_value"
                  , case commonFields.frequency of
                        Habit.EveryXDayFrequency { days, times } ->
                            Util.templater
                                (Dict.fromList [ ( "days", toString days ), ( "times", toString times ) ])
                                """{
                                type_name: "every_x_days_frequency",
                                every_x_days_frequency: {
                                    days: {{days}},
                                    times: {{times}}
                                }
                                }"""

                        Habit.TotalWeekFrequency times ->
                            Util.templater
                                (Dict.fromList [ ( "times", toString times ) ])
                                """{
                                type_name: "total_week_frequency",
                                total_week_frequency: {
                                    week: {{times}}
                                }
                                }"""

                        Habit.SpecificDayOfWeekFrequency { monday, tuesday, wednesday, thursday, friday, saturday, sunday } ->
                            Util.templater
                                (Dict.fromList
                                    [ ( "monday", toString monday )
                                    , ( "tuesday", toString tuesday )
                                    , ( "wednesday", toString wednesday )
                                    , ( "thursday", toString thursday )
                                    , ( "friday", toString friday )
                                    , ( "saturday", toString saturday )
                                    , ( "sunday", toString sunday )
                                    ]
                                )
                                """{
                                type_name: "specific_day_of_week_frequency",
                                specific_day_of_week_frequency: {
                                monday: {{monday}},
                                tuesday: {{tuesday}},
                                wednesday: {{wednesday}},
                                thursday: {{thursday}},
                                friday: {{friday}},
                                saturday: {{saturday}},
                                sunday: {{sunday}}
                                }
                                }"""
                  )
                ]

        isGoodHabit =
            case createHabit of
                Habit.CreateGoodHabit _ ->
                    True

                _ ->
                    False

        queryString =
            """mutation {
        \tadd_habit(create_habit_data: {
        \t\ttype_name: "{{type_name}}",
        \t\t{{type_name}}: {
        \t\t\tname: "{{name}}",
        \t\t\tdescription: "{{description}}",
        \t\t\t{{time_of_day}}
        \t\t\t{{frequency_name}}: {{frequency_value}}
        \t\t\tunit_name_singular: "{{unit_name_singular}}",
        \t\t\tunit_name_plural: "{{unit_name_plural}}"
        \t\t}
        \t}) {
        \t\t__typename
        \t\t...on good_habit {
        \t\t\t_id,
        \t\t\tdescription,
        \t\t\tname,
        \t\t\tsuspended,
        \t\t\ttarget_frequency {
        \t\t\t\t__typename,
        \t\t\t\t... on every_x_days_frequency {
        \t\t\t\t\tdays,
        \t\t\t\t\ttimes
        \t\t\t\t}
        \t\t\t\t... on total_week_frequency {
        \t\t\t\t\tweek
        \t\t\t\t}
        \t\t\t\t... on specific_day_of_week_frequency {
        \t\t\t\t\tmonday,
        \t\t\t\t\ttuesday,
        \t\t\t\t\twednesday,
        \t\t\t\t\tthursday,
        \t\t\t\t\tfriday,
        \t\t\t\t\tsaturday,
        \t\t\t\t\tsunday
        \t\t\t\t}
        \t\t\t},
        \t\t\ttime_of_day,
        \t\t\tunit_name_plural,
        \t\t\tunit_name_singular,
        \t\t}
        \t\t...on bad_habit {
        \t\t\t\t_id,
        \t\t\tdescription,
        \t\t\tname,
        \t\t\tsuspended,
        \t\t\tthreshold_frequency {
        \t\t\t\t__typename,
        \t\t\t\t... on every_x_days_frequency {
        \t\t\t\t\tdays,
        \t\t\t\t\ttimes
        \t\t\t\t}
        \t\t\t\t... on total_week_frequency {
        \t\t\t\t\tweek
        \t\t\t\t}
        \t\t\t\t... on specific_day_of_week_frequency {
        \t\t\t\t\tmonday,
        \t\t\t\t\ttuesday,
        \t\t\t\t\twednesday,
        \t\t\t\t\tthursday,
        \t\t\t\t\tfriday,
        \t\t\t\t\tsaturday,
        \t\t\t\t\tsunday
        \t\t\t\t}
        \t\t\t},
        \t\t\tunit_name_plural,
        \t\t\tunit_name_singular,
        \t\t}
        \t}
        }"""
                |> Util.templater templateDict
    in
        graphQLRequest queryString <| Decode.at [ "data", "add_habit" ] Habit.decodeHabit


mutationSetHabitData : YmdDate.YmdDate -> String -> Int -> String -> (ApiError -> b) -> (HabitData.HabitData -> b) -> Cmd b
mutationSetHabitData { day, month, year } habitId amount =
    let
        templateDict =
            Dict.fromList <|
                [ ( "day", toString day )
                , ( "month", toString month )
                , ( "year", toString year )
                , ( "amount", toString amount )
                , ( "habit_id", habitId )
                ]

        query =
            """mutation {
\tset_habit_data(date: { day: {{day}}, month: {{month}}, year: {{year}}}, amount: {{amount}}, habit_id: "{{habit_id}}") {
\t\t_id,
\t\tamount,
\t\tdate {
\t\t\tyear,
\t\t\tmonth,
\t\t\tday
\t\t},
\t\thabit_id
\t}
}"""
                |> Util.templater templateDict
    in
        graphQLRequest query (Decode.at [ "data", "set_habit_data" ] HabitData.decodeHabitData)

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
        \t\t\t\tunit_name_singular,
        \t\t\t\tunit_name_plural,
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
        \t\t\t\tunit_name_singular,
        \t\t\t\tunit_name_plural,
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

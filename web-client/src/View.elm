module View exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Model exposing (Model)
import Models.ApiError as ApiError
import Models.Habit as Habit
import Models.HabitData as HabitData
import Models.YmdDate as YmdDate
import Msg exposing (Msg)
import RemoteData


view : Model -> Html Msg
view model =
    div
        [ class "view" ]
        [ renderTodayPanel model.ymd model.allHabits model.allHabitData ]


renderTodayPanel :
    YmdDate.YmdDate
    -> RemoteData.RemoteData ApiError.ApiError (List Habit.Habit)
    -> RemoteData.RemoteData ApiError.ApiError (List HabitData.HabitData)
    -> Html Msg
renderTodayPanel ymd rdHabits rdHabitData =
    div
        [ class "today-panel" ]
        [ div [ class "today-panel-title" ] [ text "Todays Progress" ]
        , div [ class "today-panel-date" ] [ text <| YmdDate.prettyPrint ymd ]
        , case ( rdHabits, rdHabitData ) of
            ( RemoteData.Success habits, RemoteData.Success habitData ) ->
                let
                    renderHabit habit =
                        let
                            habitFields =
                                Habit.getCommonFields habit

                            relevantHabitData =
                                List.filter (.habitId >> (==) habitFields.id) habitData

                            habitDataForToday =
                                List.filter (.date >> (==) ymd) relevantHabitData
                                    |> List.head
                                    |> (\habitDatum ->
                                            case habitDatum of
                                                Nothing ->
                                                    0

                                                Just { amount } ->
                                                    amount
                                       )
                        in
                        div
                            [ class "habit" ]
                            [ div [ class "habit-name" ] [ text <| habitFields.name ]
                            , div
                                [ class "habit-amount-complete" ]
                                [ text <|
                                    toString habitDataForToday
                                        ++ " "
                                        ++ (if habitDataForToday == 1 then
                                                habitFields.unitNameSingular
                                            else
                                                habitFields.unitNamePlural
                                           )
                                ]
                            ]
                in
                div
                    [ class "habit-list" ]
                    (List.map renderHabit habits)

            ( RemoteData.Failure apiError, _ ) ->
                text "Failure..."

            ( _, RemoteData.Failure apiError ) ->
                text "Failure..."

            _ ->
                text "Loading..."
        ]

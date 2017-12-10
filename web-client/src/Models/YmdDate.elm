module Models.YmdDate exposing (..)

import Date
import Date.Extra as Date
import Date.Extra.Facts exposing (monthFromMonthNumber)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)


type alias YmdDate =
    { day : Int, month : Int, year : Int }


prettyPrint : YmdDate -> String
prettyPrint ymd =
    let
        prettyMonth { month } =
            case month of
                1 ->
                    "January"

                2 ->
                    "February"

                3 ->
                    "March"

                4 ->
                    "April"

                5 ->
                    "May"

                6 ->
                    "June"

                7 ->
                    "July"

                8 ->
                    "August"

                9 ->
                    "September"

                10 ->
                    "October"

                11 ->
                    "November"

                12 ->
                    "December"

                _ ->
                    "Invalid Month Number"

        prettyDay { day } =
            toString day
                ++ (if List.member day [ 1, 21, 31 ] then
                        "st"
                    else if List.member day [ 2, 22 ] then
                        "nd"
                    else if List.member day [ 3, 23 ] then
                        "rd"
                    else
                        "th"
                   )
    in
    prettyMonth ymd ++ " " ++ prettyDay ymd ++ ", " ++ toString ymd.year


{-| TODO DOC
-}
addDays : Int -> YmdDate -> YmdDate
addDays dayDelta ymd =
    Date.fromCalendarDate ymd.year (monthFromMonthNumber ymd.month) ymd.day
        |> Date.add Date.Day dayDelta
        |> fromDate


fromDate : Date.Date -> YmdDate
fromDate date =
    { year = Date.year date, month = Date.monthNumber date, day = Date.day date }


{-| TODO DOC
-}
fromString : String -> Maybe YmdDate
fromString date =
    String.split "/" date
        |> (\dateComponents ->
                case dateComponents of
                    [ day, monthNumber, shortenedYear ] ->
                        case ( String.toInt day, String.toInt monthNumber, String.toInt <| "20" ++ shortenedYear ) of
                            ( Ok day, Ok monthNumber, Ok year ) ->
                                Date.fromCalendarDate year (monthFromMonthNumber monthNumber) day
                                    |> fromDate
                                    |> Just

                            _ ->
                                Nothing

                    _ ->
                        Nothing
           )


decodeYmdDate : Decode.Decoder YmdDate
decodeYmdDate =
    decode YmdDate
        |> required "day" Decode.int
        |> required "month" Decode.int
        |> required "year" Decode.int

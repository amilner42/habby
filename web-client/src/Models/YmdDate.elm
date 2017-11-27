module Models.YmdDate exposing (..)

import Date
import Date.Extra
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)
import Set


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


fromDate : Date.Date -> YmdDate
fromDate date =
    { year = Date.year date, month = Date.Extra.monthNumber date, day = Date.day date }


decodeYmdDate : Decode.Decoder YmdDate
decodeYmdDate =
    decode YmdDate
        |> required "day" Decode.int
        |> required "month" Decode.int
        |> required "year" Decode.int

module Models.HabitData exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)
import Models.YmdDate exposing (YmdDate, decodeYmdDate)


type alias HabitData =
    { id : String, habitId : String, date : YmdDate, amount : Int }


decodeHabitData : Decode.Decoder HabitData
decodeHabitData =
    decode HabitData
        |> required "_id" Decode.string
        |> required "habit_id" Decode.string
        |> required "date" decodeYmdDate
        |> required "amount" Decode.int

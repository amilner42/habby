module Models.YmdDate exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)


type alias YmdDate =
    { day : Int, month : Int, year : Int }


decodeYmdDate : Decode.Decoder YmdDate
decodeYmdDate =
    decode YmdDate
        |> required "day" Decode.int
        |> required "month" Decode.int
        |> required "year" Decode.int

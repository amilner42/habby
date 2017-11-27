module Subscriptions exposing (subscriptions)

import Model exposing (Model)
import Msg exposing (Msg(TickMinute))
import Time


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every Time.minute TickMinute ]

module Flags exposing (Flags)

import Time


{-| The flags passed to the program from javascript upon init.
-}
type alias Flags =
    { apiBaseUrl : String, currentTime : Time.Time }

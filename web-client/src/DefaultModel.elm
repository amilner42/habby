module DefaultModel exposing (defaultModel)

import Flags exposing (Flags)
import Model exposing (Model)


defaultModel : Flags -> Model
defaultModel { apiBaseUrl } =
    { apiBaseUrl = apiBaseUrl, allHabitData = [], allHabits = [] }

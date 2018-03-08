module Msgs exposing (Msg(..))

import Date exposing (Date)


type Msg
    = SetDate Date
    | AdjustCalendar Int
    | ToggleCategory String
    | RetrieveFilteredCategories
    | FilteredCategories (List String)

module Msgs exposing (Msg(..))

import Date exposing (Date)
import Json.Decode exposing (Value)


type Msg
    = SetDate Date
    | AdjustCalendar Int
    | ToggleCategory String
    | RetrieveFilteredCategories
    | FilteredCategories (List String)
    | Search
    | SearchResults Value
    | ChangeQuery String

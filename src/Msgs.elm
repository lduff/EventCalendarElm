module Msgs exposing (Msg(..))

import Date exposing (Date)
import Material


type Msg
    = SetDate Date
    | Mdl (Material.Msg Msg)
    | AdjustCalendar Int
    | ToggleCategory String

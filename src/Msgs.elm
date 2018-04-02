module Msgs exposing (Msg(..))

import Date exposing (Date)
import Json.Decode exposing (Value)
import Models exposing (CalendarItem, CalendarView(..), ChannelView(..), UserSettings)


type Msg
    = SetDate Date
    | AdjustCalendar Int
    | ToggleCategory String
    | Search
    | SearchResults Value
    | ChangeQuery String
    | GetSources
    | Sources Value
    | SelectChannel ChannelView
    | SelectCalendarView CalendarView
    | UserSettingsResults Value
    | StartHoverIntent String
    | StartHover
    | CancelHover
    | NavigateDetail CalendarItem
    | NavigateCalendar

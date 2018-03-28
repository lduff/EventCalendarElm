module Models exposing (..)

import ColorHelper exposing (hueAndShadeToHex)
import Date exposing (Date, Day(..), day, month, year)
import Date.Extra.Create exposing (dateFromFields, getTimezoneOffset)
import Date.Extra.Duration as Duration exposing (..)
import Date.Extra.Field as Field exposing (..)
import Date.Extra.Period as Period exposing (Period(..), add)
import Dict exposing (Dict)
import Json.Decode exposing (..)
import Json.Decode.Extra exposing ((|:), withDefault)


type AnimState
    = Steady
    | Loading


type CalendarView
    = BySite
    | Combined


type ChannelView
    = Retail
    | Fulfillment
    | All


type alias Model =
    { currentDate : Date
    , animState : AnimState
    , query : String
    , categories : List Category
    , start : Date
    , end : Date
    , items : List CalendarItem
    , selectedChannel : ChannelView
    , calendarView : CalendarView
    }


type alias CalendarItem =
    { start : Date
    , end : Date
    , text : String
    , category : String
    , eventType : String
    , duration : Int
    , children : List CalendarItemChild
    , url : String
    , photo : String
    , mediaUrl : String
    , allCategories : List String
    }


type alias CalendarItemChild =
    { url : String
    , photo : String
    }


type alias Source =
    { name : String
    , channel : String
    }


type alias Category =
    { name : String
    , selected : Bool
    , hue : String
    , backgroundColor : String
    , backgroundColorDark : String
    , itemColor : String
    , lightItemColor : String
    , sortname : String
    , channel : String
    }


type alias SearchQuery =
    { query : String
    , starts : String
    , ends : String
    }


type alias UserSettings =
    { filteredCategories : List String
    , selectedChannel : String
    , selectedView : String
    }


removeTime : Date -> Date
removeTime date =
    dateFromFields (year date) (month date) (day date) 0 0 0 0


beginningOfWeek : Date -> Date
beginningOfWeek date =
    Field.fieldToDateClamp (Field.DayOfWeek ( Sun, Sun )) date
        |> Duration.add Duration.Day -7


endOfWeek : Date -> Date
endOfWeek date =
    beginningOfWeek date
        |> Duration.add Duration.Day 6


iso8601ToDate : Decoder Date.Date
iso8601ToDate =
    int
        |> andThen
            (\epochms ->
                let
                    dateAndTime =
                        epochms |> toFloat |> Date.fromTime

                    tzAdjusted =
                        Period.add Period.Hour (getTimezoneOffset dateAndTime // 60) dateAndTime
                in
                succeed <| removeTime tzAdjusted
            )


duration : Decoder Int
duration =
    map2
        (\start end ->
            diffDays end start
        )
        (field "starts" iso8601ToDate)
        (field "ends" iso8601ToDate)


calendarItemChildDecoder : Decoder CalendarItemChild
calendarItemChildDecoder =
    map2 CalendarItemChild
        (field "url" string)
        (field "photo" string)


calendarItemDecoder : Decoder CalendarItem
calendarItemDecoder =
    succeed CalendarItem
        |: field "starts" iso8601ToDate
        |: field "ends" iso8601ToDate
        |: field "title" string
        |: field "site" string
        |: field "eventType" string
        |: duration
        |: field "children" (list calendarItemChildDecoder)
        |: field "url" string
        |: (field "photo" string |> withDefault "")
        |: (field "mediaUrl" string |> withDefault "")
        |: succeed []


sourceDecoder : Decoder Source
sourceDecoder =
    succeed Source
        |: field "name" string
        |: field "channel" string


userSettingsDecoder : Decoder UserSettings
userSettingsDecoder =
    succeed UserSettings
        |: field "filteredCategories" (list string |> withDefault [])
        |: field "selectedChannel" (oneOf [ string, null <| channelViewToString Retail ])
        |: field "selectedView" (oneOf [ string, null <| calendarViewToString BySite ])


calendarViewToString : CalendarView -> String
calendarViewToString calendarView =
    case calendarView of
        BySite ->
            "by site"

        Combined ->
            "combined"


stringToCalendarView : String -> CalendarView
stringToCalendarView s =
    case s of
        "combined" ->
            Combined

        _ ->
            BySite


channelViewToString : ChannelView -> String
channelViewToString channelView =
    case channelView of
        Retail ->
            "retail"

        Fulfillment ->
            "fulfillment"

        All ->
            "all"


stringToChannelView : String -> ChannelView
stringToChannelView s =
    case s of
        "fulfillment" ->
            Fulfillment

        "all" ->
            All

        _ ->
            Retail


categoriesFromSources : List Source -> List Category
categoriesFromSources sources =
    let
        defaultCategoryValues =
            Dict.fromList
                [ ( "meh.com", { sortname = "000meh.com", hue = "green" } )
                , ( "morningsave.com", { sortname = "001morningsave.com", hue = "cyan" } )
                , ( "checkout.org", { sortname = "002checkout.org", hue = "blue-grey" } )
                , ( "casemates.com", { sortname = "003casemates.com", hue = "red" } )
                , ( "checkout.laughingsquid.com", { sortname = "004checkout.laughsquid.com", hue = "lime" } )
                ]

        defaultColor =
            "yellow"
    in
    sources
        |> List.map
            (\s ->
                case Dict.get s.name defaultCategoryValues of
                    Just defaults ->
                        { name = s.name
                        , selected = True
                        , hue = defaults.hue
                        , backgroundColor = ColorHelper.hueAndShadeToHex defaults.hue "500"
                        , backgroundColorDark = ColorHelper.hueAndShadeToHex defaults.hue "700"
                        , itemColor = ColorHelper.hueAndShadeToHex defaults.hue "300"
                        , lightItemColor = ColorHelper.hueAndShadeToHex defaults.hue "100"
                        , sortname = defaults.sortname
                        , channel = s.channel
                        }

                    Nothing ->
                        { name = s.name
                        , selected = True
                        , hue = defaultColor
                        , backgroundColor = ColorHelper.hueAndShadeToHex defaultColor "500"
                        , backgroundColorDark = ColorHelper.hueAndShadeToHex defaultColor "900"
                        , itemColor = ColorHelper.hueAndShadeToHex defaultColor "300"
                        , lightItemColor = ColorHelper.hueAndShadeToHex defaultColor "100"
                        , sortname = "999" ++ s.name
                        , channel = s.channel
                        }
            )
        |> List.sortBy .sortname


combinedCategory : Category
combinedCategory =
    let
        combinedHue =
            "teal"
    in
    { name = "All Events"
    , selected = True
    , hue = combinedHue
    , backgroundColor = ColorHelper.hueAndShadeToHex combinedHue "500"
    , backgroundColorDark = ColorHelper.hueAndShadeToHex combinedHue "900"
    , itemColor = ColorHelper.hueAndShadeToHex combinedHue "300"
    , lightItemColor = ColorHelper.hueAndShadeToHex combinedHue "100"
    , sortname = "999All Events"
    , channel = "all"
    }

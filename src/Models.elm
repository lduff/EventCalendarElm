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
import Set exposing (Set)


type AnimState
    = SlidingLeft
    | SlidingRight
    | Steady
    | Loading


type alias Model =
    { currentDate : Date
    , animState : AnimState
    , query : String
    , categories : List Category
    , start : Date
    , end : Date
    , items : List CalendarItem
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
    }


type alias CalendarItemChild =
    { url : String
    , photo : String
    }


type alias Category =
    { name : String
    , selected : Bool
    , hue : String
    , backgroundColor : String
    , backgroundColorDark : String
    , itemColor : String
    , lightItemColor : String
    }


type alias SearchQuery =
    { query : String
    , starts : String
    , ends : String
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


categories : List CalendarItem -> List Category
categories items =
    let
        categoryColors =
            Dict.fromList
                [ ( "meh.com", "green" )
                , ( "morningsave.com", "cyan" )
                , ( "checkout.org", "blue-grey" )
                , ( "casemates.com", "red" )
                , ( "checkout.laughingsquid.com", "lime" )
                ]

        defaultColor =
            "yellow"
    in
    items
        |> List.map (\c -> c.category)
        |> Set.fromList
        |> Set.toList
        |> List.map
            (\name ->
                case Dict.get name categoryColors of
                    Just hue ->
                        { name = name
                        , selected = True
                        , hue = hue
                        , backgroundColor = ColorHelper.hueAndShadeToHex hue "500"
                        , backgroundColorDark = ColorHelper.hueAndShadeToHex hue "700"
                        , itemColor = ColorHelper.hueAndShadeToHex hue "300"
                        , lightItemColor = ColorHelper.hueAndShadeToHex hue "100"
                        }

                    Nothing ->
                        { name = name
                        , selected = True
                        , hue = defaultColor
                        , backgroundColor = ColorHelper.hueAndShadeToHex defaultColor "500"
                        , backgroundColorDark = ColorHelper.hueAndShadeToHex defaultColor "900"
                        , itemColor = ColorHelper.hueAndShadeToHex defaultColor "300"
                        , lightItemColor = ColorHelper.hueAndShadeToHex defaultColor "100"
                        }
            )
        |> List.sortBy .name

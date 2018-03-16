module Models exposing (..)

import ColorHelper exposing (hueAndShadeToHex)
import Date exposing (Date)
import Date.Extra.Duration exposing (diffDays)
import Date.Extra.TimeUnit exposing (TimeUnit, startOfTime)
import Dict exposing (Dict)
import Json.Decode exposing (..)
import Regex
import Set exposing (Set)
import TestData exposing (items)


type alias CalendarModel =
    { categories : List Category
    , start : Date
    , end : Date
    , items : List CalendarItem
    }


type alias Model =
    { currentDate : Maybe Date
    , calendarModel : CalendarModel
    }


type alias CalendarItem =
    { start : Date
    , end : Date
    , text : String
    , category : String
    , duration : Int
    }


type alias Category =
    { name : String
    , selected : Bool
    , hue : String
    , backgroundColor : String
    , backgroundColorDark : String
    , itemColor : String
    }


iso8601ToDate : Decoder Date.Date
iso8601ToDate =
    string
        |> andThen
            (\string ->
                case Date.fromString string of
                    Ok dateAndTime ->
                        succeed <| startOfTime Date.Extra.TimeUnit.Day dateAndTime

                    Err error ->
                        fail error
            )


duration : Decoder Int
duration =
    map2
        (\start end ->
            diffDays end start
        )
        (field "starts" iso8601ToDate)
        (field "ends" iso8601ToDate)


calendarItem : String -> String -> String -> String -> Result String CalendarItem
calendarItem itemText itemCategory itemStart itemEnd =
    case ( Date.fromString itemStart, Date.fromString itemEnd ) of
        ( Ok startDate, Ok endDate ) ->
            Ok
                { start = startDate
                , end = endDate
                , text = itemText
                , category = itemCategory
                , duration = diffDays endDate startDate
                }

        _ ->
            Err ("date parse problem " ++ itemStart ++ " " ++ itemEnd)


calendarItemDecoder : Decoder CalendarItem
calendarItemDecoder =
    map5 CalendarItem
        (field "starts" iso8601ToDate)
        (field "ends" iso8601ToDate)
        (field "title" string)
        (field "site" string)
        duration


categories : List Category
categories =
    let
        categoryColors =
            Dict.fromList
                [ ( "meh.com", "green" )
                , ( "morningsave.com", "cyan" )
                , ( "checkout.org", "blue-grey" )
                , ( "video", "red" )
                ]

        defaultColor =
            "yellow"
    in
    testData
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
                        , backgroundColorDark = ColorHelper.hueAndShadeToHex hue "900"
                        , itemColor = ColorHelper.hueAndShadeToHex hue "300"
                        }

                    Nothing ->
                        { name = name
                        , selected = True
                        , hue = defaultColor
                        , backgroundColor = ColorHelper.hueAndShadeToHex defaultColor "500"
                        , backgroundColorDark = ColorHelper.hueAndShadeToHex defaultColor "900"
                        , itemColor = ColorHelper.hueAndShadeToHex defaultColor "300"
                        }
            )


testData : List CalendarItem
testData =
    let
        removeTabs =
            Regex.replace Regex.All (Regex.regex "\\t") (\_ -> "")

        removeBackslashes =
            Regex.replace Regex.All (Regex.regex "W\\\\Sheath") (\_ -> "")
    in
    case
        TestData.items
            |> removeBackslashes
            |> removeTabs
            |> Json.Decode.decodeString (list calendarItemDecoder)
    of
        Ok items ->
            items
                |> List.filter (\i -> i.category == "meh.com")

        Err err ->
            Debug.log err []


partitionWhile : (a -> Bool) -> List a -> ( List a, List a )
partitionWhile predicate l =
    partitionWhileInternal predicate [] l


partitionWhileInternal : (a -> Bool) -> List a -> List a -> ( List a, List a )
partitionWhileInternal predicate acc l =
    let
        nextItem =
            List.head l
    in
    case nextItem of
        Nothing ->
            ( acc, [] )

        Just n ->
            if predicate n then
                case List.tail l of
                    Nothing ->
                        ( acc ++ l, [] )

                    Just tl ->
                        partitionWhileInternal predicate (acc ++ List.singleton n) tl
            else
                ( acc, l )

module Models exposing (..)

import Date exposing (Date)
import Date.Extra.Duration exposing (diffDays)
import Date.Extra.Utils exposing (unsafeFromString)
import Material


type alias Model =
    { currentDate : Maybe Date
    , calendarData : CalendarData
    , mdl : Material.Model
    }


type alias CalendarItem =
    { start : Date
    , end : Date
    , text : String
    , category : String
    , duration : Int
    }


type alias CalendarData =
    { items : List CalendarItem
    , start : Date
    , end : Date
    }


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


testData : CalendarData
testData =
    let
        calendarItems =
            [ calendarItem "test item3" "meh.com" "7/14/2017" "7/15/2017"
            , calendarItem "test item1" "meh.com" "7/9/2017" "7/10/2017"
            , calendarItem "test item2" "morningsave.com" "7/12/2017" "7/15/2017"
            ]
                |> List.filterMap Result.toMaybe
    in
    { items = calendarItems, start = unsafeFromString "7/9/2017", end = unsafeFromString "7/15/2017" }


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

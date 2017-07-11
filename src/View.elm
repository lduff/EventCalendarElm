module View exposing (view)

import Date exposing (Date, toTime)
import Date.Extra.Compare exposing (..)
import Date.Extra.Config.Config_en_us exposing (config)
import Date.Extra.Duration as Duration exposing (..)
import Date.Extra.Format exposing (format)
import Html exposing (..)
import List
import Material.Color as Color
import Material.Grid exposing (Cell, Device(..), cell, grid, noSpacing, offset, order, size)
import Material.Options exposing (Style, css)
import Material.Scheme
import Models exposing (CalendarData, CalendarItem, Model, calendarItem, partitionWhile)
import Tuple exposing (first, second)


itemStyling : List (Style a)
itemStyling =
    [ css "text-sizing" "border-box"
    , css "text-align" "center"
    , css "padding" "4px"
    , css "border-right" "1px solid white"
    ]


view : Model -> Html msg
view model =
    let
        sortedCalendarItems =
            model.calendarData.items
                |> List.sortWith (\a b -> compare (toTime a.start) (toTime b.start))
                |> sortCalendar model.calendarData.end []
                |> List.reverse

        dayHeaderCells =
            List.range 0 6
                |> List.map (\i -> Duration.add Duration.Day i model.calendarData.start)
                |> List.map (\d -> cell [ size All 1, Color.background Color.primaryDark ] [ text <| format config "%a %e" d ])
    in
    div []
        ([ grid [ noSpacing ]
            ([ cell [ size All 1, Color.background Color.primaryDark ] [ text "prev" ]
             , cell [ size All 3, Color.background Color.primaryDark ] [ text "" ]
             ]
                ++ dayHeaderCells
                ++ [ cell [ size All 1, Color.background Color.primaryDark ] [ text "next" ] ]
            )
         ]
            ++ gridForAllItems model.calendarData.start model.calendarData.end [] sortedCalendarItems
        )
        |> Material.Scheme.topWithScheme Color.LightGreen Color.Red


gridForAllItems : Date -> Date -> List (Html msg) -> List CalendarItem -> List (Html msg)
gridForAllItems calendarStart calendarEnd acc items =
    case items of
        [] ->
            acc

        _ ->
            let
                ( grid, remaining ) =
                    gridForCalendarItems calendarStart calendarEnd items
            in
            gridForAllItems calendarStart calendarEnd (acc ++ List.singleton grid) remaining


gridForCalendarItems : Date -> Date -> List CalendarItem -> ( Html msg, List CalendarItem )
gridForCalendarItems calendarStart calendarEnd items =
    let
        ( lineItems, remaining ) =
            case items of
                [] ->
                    ( [], [] )

                [ a ] ->
                    ( [ a ], [] )

                _ ->
                    let
                        head =
                            List.take 1 items

                        ( li, remaining ) =
                            case List.tail items of
                                Nothing ->
                                    ( [], [] )

                                Just tl ->
                                    List.map2 (,) tl items
                                        |> partitionWhile (\( a, b ) -> a |> fitsAfter calendarEnd b)
                                        |> (\( a, b ) -> ( List.map first a, List.map first b ))
                    in
                    ( head ++ li, remaining )
    in
    ( grid [ noSpacing ]
        ([ cell [ Color.background Color.primaryDark, size All 4 ] [ text "foo" ] ]
            ++ (List.foldl
                    (\ci ( lastEnd, cells ) ->
                        ( ci.end, cellForCalendarItem lastEnd ci :: cells )
                    )
                    ( calendarStart, [] )
                    lineItems
                    |> second
                    |> List.reverse
               )
        )
    , remaining
    )


cellForCalendarItem : Date -> CalendarItem -> Cell a
cellForCalendarItem calendarStart item =
    let
        offsetDays =
            diffDays item.start calendarStart
    in
    cell
        ([ Color.background Color.primary
         , size All item.duration
         , offset All offsetDays
         ]
            ++ itemStyling
        )
        [ text item.text ]


fitsAfter : Date -> CalendarItem -> CalendarItem -> Bool
fitsAfter calendarEnd a b =
    is SameOrAfter b.start a.end


sortCalendar : Date -> List CalendarItem -> List CalendarItem -> List CalendarItem
sortCalendar calendarEnd acc items =
    case ( acc, items ) of
        ( [], hd :: tl ) ->
            sortCalendar calendarEnd [ hd ] tl

        ( _, [] ) ->
            acc

        ( _, [ i ] ) ->
            items ++ acc

        ( acchd :: acctail, _ ) ->
            let
                ( fits, nextItems ) =
                    List.partition (\ci -> fitsAfter calendarEnd acchd ci) items
            in
            case ( fits, nextItems ) of
                ( firstFit :: tl, n ) ->
                    sortCalendar calendarEnd (firstFit :: (acchd :: acctail)) (tl ++ n)

                ( [], hd :: tl ) ->
                    sortCalendar calendarEnd (hd :: (acchd :: acctail)) tl

                _ ->
                    acc

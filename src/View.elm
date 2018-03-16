module View exposing (view)

import Calendar exposing (calendar)
import Date exposing (Date, toTime)
import Date.Extra.Compare exposing (..)
import Date.Extra.Config.Config_en_us exposing (config)
import Date.Extra.Duration as Duration exposing (..)
import Date.Extra.Format exposing (format)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List
import Models exposing (CalendarItem, CalendarModel, Category, Model, calendarItem, partitionWhile)
import Msgs exposing (Msg(..))
import Tuple exposing (first, second)


bulmaStylesheet : Html msg
bulmaStylesheet =
    node "link"
        [ attribute "rel" "stylesheet"
        , attribute "property" "stylesheet"
        , attribute "href" "//cdnjs.cloudflare.com/ajax/libs/bulma/0.6.2/css/bulma.css"
        ]
        []


calendarStylesheet : Html msg
calendarStylesheet =
    node "link"
        [ attribute "rel" "stylesheet"
        , attribute "property" "stylesheet"
        , attribute "href" "calendar.css"
        ]
        []


fontawesome : Html msg
fontawesome =
    node "script" [ defer True, src "//use.fontawesome.com/releases/v5.0.8/js/all.js" ] []


view : Model -> Html Msg
view model =
    section [ class "section" ]
        [ div
            [ id "outer"
            , class "container"
            ]
            [ bulmaStylesheet
            , calendarStylesheet
            , fontawesome
            , viewHeader model
            , calendar model.calendarModel
            ]
        ]


checkbox : String -> Bool -> Html Msg
checkbox checkboxText checkboxSelected =
    label [ class "checkbox" ]
        [ input
            [ type_ "checkbox"
            , selected checkboxSelected
            ]
            [ text checkboxText ]
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    nav [ class "level" ]
        [ div [ class "level-left" ] [ text "EVENT CALENDAR" ]
        , div [ class "level-right" ]
            [ div [ class "field has-addons" ]
                [ div [ class "control" ]
                    [ input
                        [ class "input"
                        , type_ "text"
                        , placeholder "Find calendar items"
                        ]
                        []
                    ]
                , div [ class "control" ]
                    [ a [ class "button is-info" ]
                        [ text "Search" ]
                    ]
                ]
            ]
        ]


calendarForCategory : Category -> Date -> Date -> Dict String (List CalendarItem) -> List (Html msg)
calendarForCategory category calendarStart calendarEnd itemsDict =
    let
        itemsForCategory =
            Dict.get category.name itemsDict
    in
    case itemsForCategory of
        Nothing ->
            []

        Just items ->
            [ div
                [ class "column has-text-centered"
                , style
                    [ ( "backgroundColor", category.backgroundColor )
                    , ( "border-left", "4px solid " ++ category.backgroundColorDark )
                    ]
                ]
                [ div [] [ text category.name ]
                , div [ class "columns has-text-grey-dark has-text-weight-bold" ] <|
                    (List.range 0 6
                        |> List.map (\i -> Duration.add Duration.Day i calendarStart)
                        |> List.map
                            (\d ->
                                div [ class "column has-text-centered" ] [ text <| format config "%a %e" d ]
                            )
                    )
                ]
            ]
                ++ (items
                        |> List.sortBy (.start >> toTime)
                        |> gridForAllItems category calendarStart calendarEnd []
                   )


gridForAllItems : Category -> Date -> Date -> List (Html msg) -> List CalendarItem -> List (Html msg)
gridForAllItems category calendarStart calendarEnd acc items =
    case items of
        [] ->
            acc
                ++ [ div [ class "columns" ]
                        [ div [ class "column" ]
                            [ text "" ]
                        ]
                   ]

        _ ->
            let
                showHeader =
                    acc == []

                ( grid, remaining ) =
                    gridForNextItems category showHeader calendarStart calendarEnd items
            in
            gridForAllItems category calendarStart calendarEnd (acc ++ List.singleton grid) remaining


gridForNextItems : Category -> Bool -> Date -> Date -> List CalendarItem -> ( Html msg, List CalendarItem )
gridForNextItems category showHeader calendarStart calendarEnd items =
    let
        ( lineItems, remaining ) =
            case items of
                [] ->
                    ( [], [] )

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
    ( div
        [ class "columns is-gapless"
        , style
            [ ( "backgroundColor", category.backgroundColor )
            , ( "border-left", "4px solid " ++ category.backgroundColorDark )
            , ( "margin-bottom", "0px" )
            ]
        ]
        (List.foldl
            (\ci ( lastEnd, cells ) ->
                ( ci.end, cellForCalendarItem category lastEnd calendarEnd ci :: cells )
            )
            ( calendarStart, [] )
            lineItems
            |> second
            |> List.reverse
        )
    , remaining
    )


cellForCalendarItem : Category -> Date -> Date -> CalendarItem -> Html a
cellForCalendarItem category calendarStart calendarEnd item =
    let
        itemWidth =
            Basics.min item.duration (1 + diffDays calendarEnd calendarStart)

        offsetDays =
            Basics.max 0 (diffDays item.start calendarStart)

        continuesFromPastStyle =
            [ ( "border-left", "8px double " ++ category.backgroundColor )
            , ( "border-top-left-radius", "0px" )
            , ( "border-bottom-left-radius", "0px" )
            ]

        continuesToFutureStyle =
            [ ( "border-right", "8px double " ++ category.backgroundColor )
            , ( "border-top-right-radius", "0px" )
            , ( "border-bottom-right-radius", "0px" )
            ]
    in
    div
        [ class "column"
        , style
            ([ ( "backgroundColor", category.itemColor )
             , ( "flex-grow", toString itemWidth )
             , ( "border-radius", "3px" )
             , ( "margin", "8px" )

             --, ( "margin-left", toString (14.3 * toFloat offsetDays) ++ "%" )
             ]
                ++ (if is Before item.start calendarStart then
                        continuesFromPastStyle
                    else
                        []
                   )
                ++ (if is After item.end (Duration.add Duration.Day 1 calendarEnd) then
                        continuesToFutureStyle
                    else
                        []
                   )
            )
        ]
        [ p
            [ class "has-text-centered"
            , style
                [ ( "padding", "8px" ) ]
            ]
            [ text item.text
            , if
                is Before item.start calendarStart
                    || is After item.end (Duration.add Duration.Day 1 calendarEnd)
              then
                p [ class "is-size-7" ]
                    [ text <| format config "« %B %e" item.start
                    , text " to "
                    , text <| format config "%B %e »" item.end
                    ]
              else
                text ""
            ]
        ]


fitsAfter : Date -> CalendarItem -> CalendarItem -> Bool
fitsAfter calendarEnd a b =
    is SameOrAfter b.start a.end

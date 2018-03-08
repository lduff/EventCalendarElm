module View exposing (view)

import Date exposing (Date, toTime)
import Date.Extra.Compare exposing (..)
import Date.Extra.Config.Config_en_us exposing (config)
import Date.Extra.Duration as Duration exposing (..)
import Date.Extra.Format exposing (format)
import Dict exposing (Dict)
import Dict.Extra
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List
import Models exposing (CalendarItem, Category, Model, calendarItem, partitionWhile)
import Msgs exposing (Msg(..))
import Tuple exposing (first, second)


stylesheet : Html msg
stylesheet =
    let
        tag =
            "link"

        attrs =
            [ attribute "rel" "stylesheet"
            , attribute "property" "stylesheet"
            , attribute "href" "//cdnjs.cloudflare.com/ajax/libs/bulma/0.6.2/css/bulma.css"
            ]

        children =
            []
    in
    node tag attrs children


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
            [ stylesheet
            , fontawesome
            , viewHeader model

            --, viewDrawer model
            , viewCalendar model
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


viewDrawer : Model -> Html Msg
viewDrawer model =
    div
        []
        (model.categories
            |> List.indexedMap
                (\i c ->
                    checkbox c.name c.selected
                )
        )


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


viewCalendar : Model -> Html Msg
viewCalendar model =
    let
        calendarItemsDict : Dict String (List CalendarItem)
        calendarItemsDict =
            model.items
                |> List.filter
                    (\i ->
                        (is Before i.start model.start
                            && is After i.end model.end
                        )
                            || is3 BetweenOpenEnd i.start model.start model.end
                            || is3 BetweenOpenEnd i.end model.start model.end
                    )
                |> Dict.Extra.groupBy .category

        prevNextStyling =
            []

        dayHeaderStyling =
            []

        dayHeaderCells =
            List.range 0 6
                |> List.map (\i -> Duration.add Duration.Day i model.start)
                |> List.map
                    (\d ->
                        div [ class "column has-text-centered" ] [ text <| format config "%a %e" d ]
                    )
    in
    div
        []
        ([ div [ class "columns" ]
            [ div [ class "column is-2" ]
                [ button [ class "button is-pulled-left", onClick (AdjustCalendar -7) ]
                    [ i [ class "fas fast-backward" ] [] ]
                ]
            , div [ class "column is-8 has-text-centered" ]
                [ text <| "Week of " ++ format config "%B %e, %Y" model.start ]
            , div [ class "column is-2" ]
                [ button [ class "button is-pulled-right", onClick (AdjustCalendar 7) ]
                    [ i [ class "fas fast-forward" ] [] ]
                ]
            ]
         , div [ class "columns" ]
            ([ div [ class "column is-5" ] [ text "" ] ] ++ dayHeaderCells)
         ]
            ++ (model.categories
                    |> List.filter .selected
                    |> List.map (\c -> calendarForCategory c model.start model.end calendarItemsDict)
                    |> List.concat
               )
        )


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
            items
                |> List.sortBy (.start >> toTime)
                |> gridForAllItems category calendarStart calendarEnd []


gridForAllItems : Category -> Date -> Date -> List (Html msg) -> List CalendarItem -> List (Html msg)
gridForAllItems category calendarStart calendarEnd acc items =
    case items of
        [] ->
            acc
                ++ [ div [ class "columns" ]
                        [ div [ class "column is-12" ]
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
    ( div [ class "columns" ]
        ([ div
            [ class "column is-5"
            , style [ ( "backgroundColor", category.leftColor ) ]
            ]
            [ if showHeader then
                img [ src category.logo ] []
              else
                text ""
            ]
         ]
            ++ (List.foldl
                    (\ci ( lastEnd, cells ) ->
                        ( ci.end, cellForCalendarItem category lastEnd calendarEnd ci :: cells )
                    )
                    ( calendarStart, [] )
                    lineItems
                    |> second
                    |> List.reverse
               )
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
    in
    div
        [ class <| String.concat [ "column is-", toString itemWidth, " is-offset-", toString offsetDays ]
        , style
            [ ( "backgroundColor", category.itemColor )
            , ( "font-family", "Helvetica" )
            ]
        ]
        [ if is Before item.start calendarStart then
            span [] [ text <| format config "« %B %e" item.start ]
          else
            text ""
        , span [ class "has-text-centered" ] [ text item.text ]
        , if is After item.end (Duration.add Duration.Day 1 calendarEnd) then
            span [] [ text <| format config "%B %e »" item.end ]
          else
            text ""
        ]


fitsAfter : Date -> CalendarItem -> CalendarItem -> Bool
fitsAfter calendarEnd a b =
    is SameOrAfter b.start a.end

module View exposing (view)

import Date exposing (Date, toTime)
import Date.Extra.Compare exposing (..)
import Date.Extra.Config.Config_en_us exposing (config)
import Date.Extra.Duration as Duration exposing (..)
import Date.Extra.Format exposing (format)
import Dict exposing (Dict)
import Dict.Extra
import Html exposing (..)
import Html.Attributes exposing (src)
import List
import Material.Button as Button
import Material.Color as Color
import Material.Grid exposing (Cell, Device(..), cell, grid, noSpacing, offset, order, size)
import Material.Icon as Icon
import Material.Layout as Layout
import Material.Options as Options exposing (Style, css, styled)
import Material.Scheme
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Material.Typography as Typo
import Models exposing (CalendarItem, Category, Model, calendarItem, partitionWhile)
import Msgs exposing (Msg(..))
import Tuple exposing (first, second)


backgroundColor : Color.Color
backgroundColor =
    Color.color Color.Amber Color.S50


backgroundColorHex : String
backgroundColorHex =
    "#FFF8E1"


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader
        ]
        { header = [ viewHeader model ]
        , drawer = [ viewDrawer model ]
        , tabs = ( [], [] )
        , main = [ viewCalendar model ]
        }
        |> Material.Scheme.topWithScheme Color.BlueGrey Color.Red


viewDrawer : Model -> Html Msg
viewDrawer model =
    Options.styled div
        [ css "padding" "10px" ]
        (model.categories
            |> List.indexedMap
                (\i c ->
                    Toggles.checkbox Mdl
                        [ 1, i ]
                        model.mdl
                        [ Options.onToggle <| ToggleCategory c.name
                        , Toggles.ripple
                        , Toggles.value c.selected
                        ]
                        [ text c.name ]
                )
        )


viewHeader : Model -> Html Msg
viewHeader model =
    Options.styled div
        [ css "display" "flex"
        , css "align-items" "center"
        , css "justify-content" "flex-start"
        , css "font-family" "Roboto,Helvetica"
        , css "padding-left" "80px"
        , Typo.contrast 1.0
        ]
        [ Options.styled span [] [ text "EVENT CALENDAR" ]
        , Options.styled div
            [ css "display" "flex"
            , css "align-items" "center"
            , css "justify-content" "center"
            , css "margin-left" "20px"
            , css "margin-top" "8px"
            , css "margin-bottom" "8px"
            , css "padding-left" "4px"
            , css "padding-right" "4px"
            , css "border-radius" "4px"
            , Color.background (Color.color Color.BlueGrey Color.S200)
            ]
            [ Button.render Mdl
                [ 3 ]
                model.mdl
                [ Button.icon
                , Button.ripple
                ]
                [ Icon.i "search" ]
            , Textfield.render Mdl
                [ 2 ]
                model.mdl
                [ css "padding" "8px"
                ]
                []
            , Options.styled span [ css "width" "10px" ] [ text "" ]
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
            [ css "display" "flex"
            , css "align-items" "center"
            , css "align-content" "center"
            , css "justify-content" "center"
            , css "font-family" "Roboto,Helvetica"
            , css "color" "white"
            , Typo.subhead
            , Typo.contrast 1.0
            ]

        dayHeaderStyling =
            [ css "text-sizing" "border-box"
            , Typo.center
            , css "padding" "4px"
            , css "color" "white"
            , Typo.subhead
            , Typo.contrast 1.0
            ]

        dayHeaderCells =
            List.range 0 6
                |> List.map (\i -> Duration.add Duration.Day i model.start)
                |> List.map
                    (\d ->
                        cell ([ size All 1, Color.background Color.primaryDark, css "width" "12%" ] ++ dayHeaderStyling)
                            [ text <| format config "%a %e" d ]
                    )
    in
    Options.styled div
        [ Color.background backgroundColor ]
        ([ grid [ noSpacing ]
            [ cell ([ size All 12, Color.background Color.primaryDark ] ++ prevNextStyling)
                [ Button.render Mdl
                    [ 0, 0 ]
                    model.mdl
                    [ Button.minifab
                    , Button.ripple
                    , Options.onClick (AdjustCalendar -7)
                    ]
                    [ Icon.i "fast_rewind" ]
                , text <| "Week of " ++ format config "%B %e, %Y" model.start
                , Button.render Mdl
                    [ 0, 1 ]
                    model.mdl
                    [ Button.minifab
                    , Button.ripple
                    , Options.onClick (AdjustCalendar 7)
                    ]
                    [ Icon.i "fast_forward" ]
                ]
            ]
         , grid [ noSpacing ]
            ([ cell [ size All 5, Color.background Color.primaryDark, css "width" "16%" ] [ text "" ]
             ]
                ++ dayHeaderCells
            )
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
                ++ [ grid []
                        [ cell
                            [ Color.background backgroundColor
                            , size All 12
                            , css "height" "1px"
                            ]
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
    ( grid [ noSpacing ]
        ([ cell
            [ Color.background category.leftColor
            , size All 5
            , css "display" "flex"
            , css "align-items" "center"
            , css "justify-content" "center"
            , css "width" "16%"
            , css "border-right" <| "4px solid " ++ category.leftColorDarkHex
            ]
            [ if showHeader then
                Options.styled_ img
                    [ css "height" "20px"
                    , css "padding" "4px"
                    ]
                    [ src category.logo ]
                    []
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


cellForCalendarItem : Category -> Date -> Date -> CalendarItem -> Cell a
cellForCalendarItem category calendarStart calendarEnd item =
    let
        itemWidth =
            min item.duration (1 + diffDays calendarEnd calendarStart)

        itemStyling : List (Style a)
        itemStyling =
            [ css "display" "flex"
            , css "align-items" "center"
            , css "align-content" "center"
            , css "justify-content" "center"
            , css "text-sizing" "border-box"
            , css "padding" "4px"
            , css "font-family" "Roboto,Helvetica"
            , css "border-left" <| "1px solid " ++ category.leftColorDarkHex
            , css "border-bottom" <| "1px solid " ++ backgroundColorHex
            , css "width" <| (toString <| 12 * toFloat itemWidth) ++ "%"
            ]

        offsetDays =
            max 0 (diffDays item.start calendarStart)
    in
    cell
        ([ Color.background category.itemColor
         , size All itemWidth
         , css "margin-left" <| (toString <| 12 * toFloat offsetDays) ++ "%"

         {- offset All offsetDays -}
         ]
            ++ itemStyling
        )
        [ if is Before item.start calendarStart then
            Options.styled span [ Typo.caption ] [ text <| format config "« %B %e" item.start ]
          else
            text ""
        , Options.styled span [ css "width" "86%", css "text-align" "center" ] [ text item.text ]
        , if is After item.end (Duration.add Duration.Day 1 calendarEnd) then
            Options.styled span [ Typo.caption ] [ text <| format config "%B %e »" item.end ]
          else
            text ""
        ]


fitsAfter : Date -> CalendarItem -> CalendarItem -> Bool
fitsAfter calendarEnd a b =
    is SameOrAfter b.start a.end

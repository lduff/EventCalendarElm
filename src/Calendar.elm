module Calendar exposing (..)

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
import Models exposing (CalendarItem, CalendarModel, Category)
import Msgs exposing (Msg(..))


calendar : CalendarModel -> Html Msg
calendar model =
    let
        calendarItemsDict : Dict String (List CalendarItem)
        calendarItemsDict =
            model.items
                |> List.filter
                    (\i ->
                        (is Before i.start model.start
                            && is After i.end model.end
                        )
                            || is3 BetweenOpenStart i.start model.start model.end
                            || is3 BetweenOpenEnd i.end model.start model.end
                    )
                |> Dict.Extra.groupBy .category
    in
    div [ class "calendar" ]
        ([ calendarHeader model
         ]
            ++ (model.categories
                    |> List.map
                        (\c ->
                            case Dict.get c.name calendarItemsDict of
                                Just items ->
                                    category c model.start model.end items

                                Nothing ->
                                    text ""
                        )
               )
        )


calendarHeader : CalendarModel -> Html Msg
calendarHeader model =
    div [ class "calendar-header" ]
        [ div [ class "calendar-header-prev" ]
            [ button [ class "button", onClick (AdjustCalendar -7) ]
                [ i [ class "fas fa-fast-backward" ] [] ]
            ]
        , div [ class "calendar-header-date" ]
            [ text <| "Week of " ++ format config "%B %e, %Y" model.start ]
        , div [ class "calendar-header-next" ]
            [ button [ class "button", onClick (AdjustCalendar 7) ]
                [ i [ class "fas fa-fast-forward" ] [] ]
            ]
        ]


category : Category -> Date -> Date -> List CalendarItem -> Html Msg
category category calendarStart calendarEnd itemsForCategory =
    div [ class "calendar-category" ]
        [ div
            [ class "calendar-category-header"
            , style
                [ ( "backgroundColor", category.backgroundColor )
                , ( "border-left", "4px solid " ++ category.backgroundColorDark )
                ]
            ]
            ([ div [ class "calendar-category-name" ] [ text category.name ] ]
                ++ (List.range 0 6
                        |> List.map (\i -> Duration.add Duration.Day i calendarStart)
                        |> List.map
                            (\d ->
                                div [ class "calendar-category-day-header has-text-grey-dark has-text-weight-bold has-text-centered" ]
                                    [ text <| format config "%a %e" d ]
                            )
                   )
            )
        , itemsForCategory
            |> List.sortBy (.start >> toTime)
            |> categoryItems category calendarStart calendarEnd
        ]


categoryItems : Category -> Date -> Date -> List CalendarItem -> Html Msg
categoryItems category calendarStart calendarEnd items =
    div [ class "calendar-category-items" ] <| List.map (\item -> calendarItem calendarStart calendarEnd item) items


calendarItem : Date -> Date -> CalendarItem -> Html Msg
calendarItem calendarStart calendarEnd item =
    div
        [ class "calendar-item" ]
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

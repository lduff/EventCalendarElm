module Calendar exposing (..)

import Date exposing (Date, toTime)
import Date.Extra.Compare exposing (..)
import Date.Extra.Config.Config_en_us exposing (config)
import Date.Extra.Duration as Duration exposing (..)
import Date.Extra.Format exposing (format, isoString)
import Dict exposing (Dict)
import Dict.Extra
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
import Models exposing (AnimState(..), CalendarItem, CalendarItemChild, Category, Model, removeTime)
import Msgs exposing (Msg(..))


calendar : Model -> Html Msg
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
                            || is3 BetweenOpenEnd i.start model.start model.end
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
                                    category model c items

                                Nothing ->
                                    text ""
                        )
               )
        )


calendarHeader : Model -> Html Msg
calendarHeader model =
    div [ class "calendar-header" ]
        [ div [ class "calendar-header-prev" ]
            [ button [ class "button is-white", onClick (AdjustCalendar -7) ]
                [ i [ class "fas fa-fast-backward" ] [] ]
            ]
        , div [ class "calendar-header-date" ]
            [ text <| "Week of " ++ format config "%B %e, %Y" model.start ]
        , div [ class "calendar-header-next" ]
            [ button [ class "button is-white", onClick (AdjustCalendar 7) ]
                [ i [ class "fas fa-fast-forward" ] [] ]
            ]
        ]


category : Model -> Category -> List CalendarItem -> Html Msg
category model category itemsForCategory =
    div
        [ class
            ("calendar-category"
                ++ (case model.animState of
                        SlidingLeft ->
                            " slide-left"

                        SlidingRight ->
                            " slide-right"

                        _ ->
                            ""
                   )
            )
        ]
        [ div
            [ class "calendar-category-name has-text-weight-bold is-size-4"
            , style
                [ ( "backgroundColor", category.backgroundColor )
                , ( "border-left", "4px solid " ++ category.backgroundColorDark )
                , ( "color", category.lightItemColor )
                ]
            ]
            [ text category.name ]
        , div
            [ class "calendar-category-header"
            , style
                [ ( "backgroundColor", category.backgroundColor )
                , ( "border-left", "4px solid " ++ category.backgroundColorDark )
                ]
            ]
          <|
            (List.range 0 6
                |> List.map (\i -> Duration.add Duration.Day i model.start)
                |> List.map
                    (\d ->
                        let
                            dayCssClass =
                                if removeTime d == removeTime model.currentDate then
                                    " calendar-category-day-header-current-date"
                                else
                                    ""
                        in
                        div
                            [ class
                                ("calendar-category-day-header has-text-grey-dark has-text-weight-bold has-text-centered"
                                    ++ dayCssClass
                                )
                            ]
                            [ text <| format config "%a %e" d ]
                    )
            )
        , itemsForCategory
            |> List.sortBy (\i -> i.eventType ++ isoString i.start)
            |> categoryItems category model.start model.end
        ]


categoryItems : Category -> Date -> Date -> List CalendarItem -> Html Msg
categoryItems category calendarStart calendarEnd items =
    div
        [ class "calendar-category-items"
        , style
            [ ( "backgroundColor", category.backgroundColor )
            , ( "border-left", "4px solid " ++ category.backgroundColorDark )
            ]
        ]
    <|
        List.map (\item -> calendarItem category calendarStart calendarEnd item) items


calendarItem : Category -> Date -> Date -> CalendarItem -> Html Msg
calendarItem category calendarStart calendarEnd item =
    let
        itemStart =
            Basics.max 1 (1 + diffDays item.start calendarStart)

        itemEnd =
            Basics.min (2 + diffDays calendarEnd calendarStart) (1 + diffDays item.end calendarStart)

        continuesFromPrev =
            is Before item.start calendarStart

        continuesToNext =
            is After item.end (Duration.add Duration.Day 1 calendarEnd)

        iconClass =
            case item.eventType of
                "catalog" ->
                    "fas fa-dollar-sign"

                "poll" ->
                    "fas fa-chart-bar"

                "video" ->
                    "fas fa-video"

                _ ->
                    ""

        startEndText =
            case ( continuesFromPrev, continuesToNext ) of
                ( True, True ) ->
                    format config "« %B %e" item.start
                        ++ " through "
                        ++ format config "%B %e »" (Duration.add Duration.Day -1 item.end)

                ( True, False ) ->
                    "« from " ++ format config "%B %e" item.start

                ( False, True ) ->
                    "through " ++ format config "%B %e »" (Duration.add Duration.Day -1 item.end)

                ( False, False ) ->
                    ""
    in
    div
        [ class
            ("calendar-item is-size-7 has-text-centered"
                ++ (if continuesFromPrev then
                        " calendar-item-continues-from-prev"
                    else
                        ""
                   )
                ++ (if continuesToNext then
                        " calendar-item-continues-to-next"
                    else
                        ""
                   )
            )
        , style
            [ ( "backgroundColor", category.itemColor )
            , ( "border-color", category.backgroundColor )
            , ( "grid-column-start", toString itemStart )
            , ( "grid-column-end", toString itemEnd )
            ]
        ]
        [ span [ class "has-text-weight-semibold" ] [ text item.text ]
        , div [ class "calendar-item-card" ]
            [ if continuesFromPrev || continuesToNext then
                p [ class "is-size-7" ] [ text startEndText ]
              else
                text ""
            , div [ class "calendar-item-photo" ]
                [ if not <| String.isEmpty item.photo then
                    img [ src item.photo ] []
                  else
                    text ""
                ]
            , div [ class "calendar-item-buttons" ]
                [ if not <| String.isEmpty item.mediaUrl then
                    a
                        [ href item.mediaUrl
                        , target "_blank"
                        , class "button is-primary is-small"
                        ]
                        [ text "Preview" ]
                  else
                    text ""
                , a
                    [ href item.url
                    , class "button is-link is-small"
                    ]
                    [ text "Edit" ]

                --, a
                --    [ href "#"
                --    , class "button is-info is-small"
                --    ]
                --    [ text "Report" ]
                ]
            ]
        , i
            [ class ("calendar-item-icon " ++ iconClass)
            , title item.eventType
            ]
            []
        ]

module Calendar exposing (..)

import Date.Extra.Compare exposing (..)
import Date.Extra.Config.Config_en_us exposing (config)
import Date.Extra.Duration as Duration exposing (..)
import Date.Extra.Format exposing (format, isoString)
import Dict exposing (Dict)
import Dict.Extra
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onMouseEnter, onMouseLeave)
import List.Extra
import Models exposing (CalendarItem, CalendarItemChild, CalendarView(..), Category, ChannelView(..), HoverIntent(..), Model, PageState(..), removeTime)
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
    div [ class "calendar" ] <|
        case model.calendarView of
            BySite ->
                calendarHeader model
                    :: (model.categories
                            |> List.filter
                                (\c ->
                                    model.selectedChannel
                                        == All
                                        || (c.channel == "retail" && model.selectedChannel == Retail && c.selected)
                                        || (c.channel == "fulfillment" && model.selectedChannel == Fulfillment && c.selected)
                                )
                            |> List.map
                                (\c ->
                                    case Dict.get c.name calendarItemsDict of
                                        Just items ->
                                            category model c items

                                        Nothing ->
                                            text ""
                                )
                       )

            Combined ->
                [ calendarHeader model
                , model.items
                    |> List.filter
                        (\i ->
                            let
                                ( channel, selected ) =
                                    case List.Extra.find (\c -> c.name == i.category) model.categories of
                                        Just category ->
                                            ( category.channel, category.selected )

                                        _ ->
                                            ( "all", True )
                            in
                            model.selectedChannel
                                == All
                                || (channel == "retail" && model.selectedChannel == Retail && selected)
                                || (channel == "fulfillment" && model.selectedChannel == Fulfillment && selected)
                        )
                    |> List.Extra.uniqueBy (\i -> i.url)
                    |> category model Models.combinedCategory
                ]


calendarHeader : Model -> Html Msg
calendarHeader model =
    div [ class "calendar-header" ]
        [ div [ class "calendar-header-prev" ]
            [ button [ class "button is-white", onClick (AdjustCalendar -7) ]
                [ i [ class "fas fa-angle-double-left" ] [] ]
            ]
        , div [ class "calendar-header-date" ]
            [ text <| "Week of " ++ format config "%B %e, %Y" model.start ]
        , div [ class "calendar-header-next" ]
            [ button [ class "button is-white", onClick (AdjustCalendar 7) ]
                [ i [ class "fas fa-angle-double-right" ] [] ]
            ]
        ]


category : Model -> Category -> List CalendarItem -> Html Msg
category model category itemsForCategory =
    div
        [ class "calendar-category" ]
        [ div
            [ class "calendar-category-name"
            , style
                [ ( "backgroundColor", category.backgroundColor )
                , ( "border-left", "4px solid " ++ category.backgroundColorDark )
                , ( "color", category.lightItemColor )
                ]
            ]
            [ span [ class "has-text-weight-bold is-size-4" ] [ text category.name ]
            , if category.name /= "All Events" then
                a
                    [ class "button is-small is-rounded is-info"
                    , style
                        [ ( "font-size", "8px" )
                        , ( "vertical-align", "text-top" )
                        , ( "margin-left", "10px" )
                        ]
                    , onClick <| ToggleCategory category.name
                    ]
                <|
                    if category.selected then
                        [ span [ class "icon is-small" ]
                            [ i [ class "fas fa-eye-slash" ] []
                            ]
                        , span [] [ text "hide" ]
                        ]
                    else
                        [ span [ class "icon is-small" ]
                            [ i [ class "fas fa-eye" ] []
                            ]
                        , span [] [ text "show" ]
                        ]
              else
                text ""
            ]
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
            |> List.sortBy (\i -> i.category ++ i.eventType ++ isoString i.start)
            |> categoryItems model category
        ]


categoryItems : Model -> Category -> List CalendarItem -> Html Msg
categoryItems model category items =
    div
        [ class "calendar-category-items"
        , style
            [ ( "backgroundColor", category.backgroundColor )
            , ( "border-left", "4px solid " ++ category.backgroundColorDark )
            ]
        ]
    <|
        List.map (\item -> calendarItem model category item) items


calendarItem : Model -> Category -> CalendarItem -> Html Msg
calendarItem model category item =
    let
        itemStart =
            Basics.max 1 (1 + diffDays item.start model.start)

        itemEnd =
            Basics.min (2 + diffDays model.end model.start) (1 + diffDays item.end model.start)

        continuesFromPrev =
            is Before (removeTime item.start) (removeTime model.start)

        continuesToNext =
            is After item.end (Duration.add Duration.Day 1 model.end)

        iconClass =
            case item.eventType of
                "catalog" ->
                    "fas fa-dollar-sign"

                "poll" ->
                    "fas fa-chart-bar"

                "video" ->
                    "fas fa-video"

                "mehrathon" ->
                    "fas fa-meh"

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

        ( itemColor, backgroundColor ) =
            case model.calendarView of
                BySite ->
                    ( category.itemColor, category.backgroundColor )

                Combined ->
                    case List.Extra.find (\c -> c.name == item.category) model.categories of
                        Just itemCategory ->
                            ( itemCategory.itemColor, itemCategory.backgroundColor )

                        _ ->
                            ( category.itemColor, category.backgroundColor )

        isHoverItem =
            case model.hoverIntent of
                Hover hoverId ->
                    category.name ++ item.url == hoverId

                Intent _ ->
                    False

                None ->
                    False
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
                ++ (if isHoverItem then
                        " calendar-item-hover"
                    else
                        ""
                   )
            )
        , style
            [ ( "backgroundColor", itemColor )
            , ( "border-color", backgroundColor )
            , ( "grid-column-start", toString itemStart )
            , ( "grid-column-end", toString itemEnd )
            ]
        , onMouseEnter <| StartHoverIntent (category.name ++ item.url)
        , onMouseLeave <| CancelHover
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
            , if model.calendarView == Combined || List.length item.allCategories > 1 then
                tagsForCategories model item.allCategories
              else
                text ""
            , div [ class "calendar-item-buttons buttons" ]
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
                , if item.eventType == "mehrathon" then
                    a
                        [ href "#"
                        , class "button is-small"
                        , onClick <| NavigateDetail item
                        ]
                        [ text "Details" ]
                  else
                    text ""
                ]
            ]
        , i
            [ class ("calendar-item-icon " ++ iconClass)
            , title item.eventType
            ]
            []
        ]


tagsForCategories : Model -> List String -> Html Msg
tagsForCategories model categories =
    categories
        |> List.map
            (\c ->
                let
                    ( textColor, bgColor ) =
                        case model.categories |> List.Extra.find (\cat -> cat.name == c) of
                            Just c ->
                                ( c.lightItemColor, c.backgroundColor )

                            _ ->
                                ( "", "" )
                in
                span
                    [ class "tag"
                    , style
                        [ ( "color", textColor )
                        , ( "background-color", bgColor )
                        ]
                    ]
                    [ text c ]
            )
        |> div [ class "tags" ]

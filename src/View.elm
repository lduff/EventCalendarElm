module View exposing (view)

import Calendar exposing (calendar)
import Date.Extra.Config.Config_en_us exposing (config)
import Date.Extra.Format exposing (format, isoString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput)
import Html.Events.Extra exposing (onEnter)
import Models exposing (CalendarItem, CalendarView(..), ChannelView(..), Model, PageState(..), calendarViewToString, channelViewToString)
import Msgs exposing (Msg(..))


view : Model -> Html Msg
view model =
    div
        [ id "outer"
        , class "container"
        ]
    <|
        case model.pageState of
            Loading ->
                [ loading model ]

            Calendar ->
                [ viewHeader model
                , calendar model
                ]

            Detail ->
                [ detail model ]


viewNavbar : Model -> Html Msg
viewNavbar model =
    nav [ class "navbar", attribute "role" "navigation", style [ ( "margin-bottom", "8px" ) ] ]
        [ div [ class "navbar-brand" ]
            [ a [ class "navbar-item", href "https://mediocre.my.salesforce.com" ]
                [ img [ src "https://s3.amazonaws.com/mediocre-static/logo.png", alt "Mediocre", height 28 ] [] ]
            ]
        , div [ class "navbar-menu" ]
            [ div [ class "navbar-start" ]
                [ a [ class "navbar-item", href "https://mediocre.my.salesforce.com" ]
                    [ text "Back to Salesforce" ]
                ]
            ]
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    nav [ class "level", style [ ( "margin-top", "8px" ) ] ]
        [ div [ class "level-left" ]
            [ div
                [ class "level-item"
                , style
                    [ ( "font-size", "24px" )
                    , ( "font-weight", "600" )
                    ]
                ]
                [ text "Event Calendar" ]
            ]
        , div [ class "level-item" ]
            [ channelDropdown model ]
        , div [ class "level-item" ]
            [ viewDropdown model ]
        , div [ class "level-right" ]
            [ div [ class "level-item" ]
                [ div [ class "field has-addons" ]
                    [ div [ class "control" ]
                        [ input
                            [ class "input"
                            , type_ "text"
                            , placeholder "Find calendar items"
                            , value model.query
                            , onInput ChangeQuery
                            , onEnter Search
                            ]
                            []
                        ]
                    , div [ class "control" ]
                        [ button
                            [ class "button is-info"
                            , onClick Search
                            ]
                            [ text "Search" ]
                        ]
                    ]
                ]
            ]
        ]


loading : Model -> Html Msg
loading model =
    div [ class "loading" ]
        [ i [ class "fas fa-flask" ] [] ]


channelDropdown : Model -> Html Msg
channelDropdown model =
    div [ class "dropdown is-hoverable" ]
        [ div [ class "dropdown-trigger" ]
            [ button [ class "button" ]
                [ span []
                    [ text <| channelViewToString model.selectedChannel ]
                , span [ class "icon is-small" ] [ i [ class "fas fa-angle-down" ] [] ]
                ]
            ]
        , div [ class "dropdown-menu", attribute "role" "menu" ]
            [ div [ class "dropdown-content" ]
                [ a [ class "dropdown-item", href "#", onClick <| SelectChannel Retail ] [ text "retail" ]
                , a [ class "dropdown-item", href "#", onClick <| SelectChannel Fulfillment ] [ text "fulfillment" ]
                , a [ class "dropdown-item", href "#", onClick <| SelectChannel All ] [ text "all" ]
                ]
            ]
        ]


viewDropdown : Model -> Html Msg
viewDropdown model =
    div [ class "dropdown is-hoverable" ]
        [ div [ class "dropdown-trigger" ]
            [ button [ class "button" ]
                [ span []
                    [ text <| calendarViewToString model.calendarView ]
                , span [ class "icon is-small" ] [ i [ class "fas fa-angle-down" ] [] ]
                ]
            ]
        , div [ class "dropdown-menu", attribute "role" "menu" ]
            [ div [ class "dropdown-content" ]
                [ a [ class "dropdown-item", href "#", onClick <| SelectCalendarView BySite ] [ text "by site" ]
                , a [ class "dropdown-item", href "#", onClick <| SelectCalendarView Combined ] [ text "combined" ]
                ]
            ]
        ]


detail : Model -> Html Msg
detail model =
    div [ class "detail" ]
        [ detailHeader model
        , case model.detailItem of
            Just item ->
                detailItem model item

            Nothing ->
                text ""
        ]


detailHeader : Model -> Html Msg
detailHeader model =
    div []
        [ span [ class "navbar-menu" ]
            [ a [ class "navbar-item", href "#", onClick NavigateCalendar ]
                [ text "Back to Calendar" ]
            ]
        ]


detailItem : Model -> CalendarItem -> Html Msg
detailItem model item =
    let
        children =
            item.children
                |> List.sortBy (\c -> isoString c.start)
    in
    div []
        [ span [ class "title" ] [ text item.text ]
        , table [ class "table" ]
            [ thead []
                [ th [] []
                , th [] []
                , th [] [ text "starts" ]
                , th [] [ text "ends" ]
                , th [] []
                ]
            , tbody [] <|
                (children
                    |> List.map
                        (\i ->
                            tr []
                                [ td [] [ img [ src i.photo, style [ ( "height", "20px" ) ] ] [] ]
                                , td [] [ text i.text ]
                                , td [] [ text <| format config "%-I:%M %P" i.start ]
                                , td [] [ text <| format config "%-I:%M %P" i.end ]
                                , td []
                                    [ a
                                        [ href i.url
                                        , class "button is-link is-small"
                                        ]
                                        [ text "Edit" ]
                                    ]
                                ]
                        )
                )
            ]
        ]

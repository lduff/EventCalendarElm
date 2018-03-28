module View exposing (view)

import Calendar exposing (calendar)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput)
import Html.Events.Extra exposing (onEnter)
import Models exposing (AnimState(..), CalendarView(..), ChannelView(..), Model, calendarViewToString, channelViewToString)
import Msgs exposing (Msg(..))


view : Model -> Html Msg
view model =
    section [ class "section" ]
        [ div
            [ id "outer"
            , class "container"
            ]
            [ viewHeader model
            , case model.animState of
                Loading ->
                    loading model

                _ ->
                    calendar model
            ]
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    nav [ class "level" ]
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

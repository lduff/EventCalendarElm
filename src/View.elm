module View exposing (view)

import Calendar exposing (calendar)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput)
import Models exposing (AnimState(..), Model)
import Msgs exposing (Msg(..))


view : Model -> Html Msg
view model =
    section [ class "section" ]
        [ div
            [ id "outer"
            , class "container"
            ]
            [ --bulmaStylesheet
              --, calendarStylesheet
              --, fontawesome
              --,
              viewHeader model
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
        [ div [ class "level-left title is-4" ] [ text "Event Calendar" ]
        , div [ class "level-right" ]
            [ div [ class "field has-addons" ]
                [ div [ class "control" ]
                    [ input
                        [ class "input"
                        , type_ "text"
                        , placeholder "Find calendar items"
                        , value model.query
                        , onInput ChangeQuery
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


loading : Model -> Html Msg
loading model =
    div [ class "loading" ]
        [ i [ class "fas fa-flask" ] [] ]

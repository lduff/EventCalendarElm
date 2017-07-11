module Main exposing (main)

import Date exposing (..)
import Html exposing (..)
import Material
import Models exposing (Model, testData)
import Msgs exposing (Msg(..))
import Task
import Update exposing (update)
import View exposing (view)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( { currentDate = Nothing
      , calendarData = testData
      , mdl = Material.model
      }
    , Task.perform SetDate Date.now
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []

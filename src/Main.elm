module Main exposing (main)

import Date exposing (..)
import Date.Extra.Utils exposing (unsafeFromString)
import Html exposing (..)
import Material
import Models exposing (Model)
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
      , categories = Models.categories
      , mdl = Material.model
      , items = Models.testData
      , start = unsafeFromString "7/9/2017"
      , end = unsafeFromString "7/15/2017"
      }
    , Task.perform SetDate Date.now
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []

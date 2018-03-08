module Main exposing (main)

import Date exposing (..)
import Date.Extra.Utils exposing (unsafeFromString)
import Html exposing (..)
import Models exposing (Model)
import Msgs exposing (Msg(..))
import Ports exposing (..)
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
      , items = Models.testData
      , start = unsafeFromString "7/9/2017"
      , end = unsafeFromString "7/15/2017"
      }
    , Cmd.batch
        [ Task.perform SetDate Date.now
        , retrieveFilteredCategories ()
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ filteredCategories Msgs.FilteredCategories ]

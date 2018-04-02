module Main exposing (main)

import Date exposing (..)
import Date.Extra.Utils exposing (unsafeFromString)
import Html exposing (..)
import Models exposing (CalendarView(..), ChannelView(..), HoverIntent(..), Model, PageState(..))
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
    ( { currentDate = unsafeFromString "1/1/2017"
      , categories = []
      , items = []
      , start = unsafeFromString "1/1/2017"
      , end = unsafeFromString "1/1/2017"
      , query = ""
      , pageState = Loading
      , selectedChannel = Retail
      , calendarView = BySite
      , hoverIntent = None
      , detailItem = Nothing
      }
    , Cmd.batch
        [ Task.perform SetDate Date.now
        , getSources ()
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ userSettings Msgs.UserSettingsResults
        , searchResults Msgs.SearchResults
        , sources Msgs.Sources
        ]

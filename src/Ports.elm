port module Ports exposing (..)

import Json.Decode exposing (Value)
import Models exposing (SearchQuery, UserSettings)


port userSettings : (Value -> msg) -> Sub msg


port getUserSettings : () -> Cmd msg


port saveUserSettings : UserSettings -> Cmd msg


port search : SearchQuery -> Cmd msg


port searchResults : (Value -> msg) -> Sub msg


port getSources : () -> Cmd msg


port sources : (Value -> msg) -> Sub msg

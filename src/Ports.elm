port module Ports exposing (..)

import Json.Decode exposing (Value)
import Models exposing (SearchQuery)


port retrieveFilteredCategories : () -> Cmd msg


port filteredCategories : (List String -> msg) -> Sub msg


port storeFilteredCategories : List String -> Cmd msg


port search : SearchQuery -> Cmd msg


port searchResults : (Value -> msg) -> Sub msg

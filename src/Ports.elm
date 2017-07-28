port module Ports exposing (..)


port retrieveFilteredCategories : () -> Cmd msg


port filteredCategories : (List String -> msg) -> Sub msg


port storeFilteredCategories : List String -> Cmd msg

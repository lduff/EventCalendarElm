module Update exposing (update)

import Date.Extra.Duration exposing (..)
import Models exposing (Model)
import Msgs exposing (Msg(..))
import Ports exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetDate d ->
            ( { model | currentDate = Just d }, Cmd.none )

        AdjustCalendar days ->
            ( { model
                | start = add Day days model.start
                , end = add Day days model.end
              }
            , Cmd.none
            )

        ToggleCategory toggleCategory ->
            let
                updatedCategories =
                    model.categories
                        |> List.map
                            (\c ->
                                if c.name == toggleCategory then
                                    { c | selected = not c.selected }
                                else
                                    c
                            )
            in
            ( { model | categories = updatedCategories }
            , storeFilteredCategories
                (updatedCategories
                    |> List.filter (not << .selected)
                    |> List.map .name
                )
            )

        RetrieveFilteredCategories ->
            ( model, retrieveFilteredCategories () )

        FilteredCategories categories ->
            let
                filteredCategories =
                    model.categories
                        |> List.map
                            (\c ->
                                if List.member c.name categories then
                                    { c | selected = False }
                                else
                                    { c | selected = True }
                            )
            in
            ( { model | categories = filteredCategories }, Cmd.none )

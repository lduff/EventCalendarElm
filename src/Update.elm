module Update exposing (update)

import Date.Extra.Duration exposing (..)
import Date.Extra.Format as Format
import Json.Decode exposing (decodeValue, list)
import Models exposing (AnimState(..), Model, beginningOfWeek, calendarItemDecoder, categories, endOfWeek)
import Msgs exposing (Msg(..))
import Ports exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetDate d ->
            let
                newStart =
                    beginningOfWeek d

                newEnd =
                    endOfWeek d
            in
            ( { model
                | currentDate = d
                , start = newStart
                , end = newEnd
              }
            , search <| Models.SearchQuery "" (Format.isoString newStart) (Format.isoString newEnd)
            )

        AdjustCalendar days ->
            let
                newStart =
                    add Day days model.start

                newEnd =
                    add Day days model.end
            in
            ( { model
                | start = newStart
                , end = newEnd
                , items = []
                , animState = Loading
              }
            , search <| Models.SearchQuery model.query (Format.isoString newStart) (Format.isoString newEnd)
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
                updatedCategories =
                    model.categories
                        |> List.map
                            (\c ->
                                if List.member c.name categories then
                                    { c | selected = False }
                                else
                                    { c | selected = True }
                            )
            in
            ( { model | categories = updatedCategories }, Cmd.none )

        Search ->
            ( { model | animState = Loading }
            , search <| Models.SearchQuery model.query (Format.isoString model.start) (Format.isoString model.end)
            )

        SearchResults value ->
            let
                newItems =
                    case decodeValue (list calendarItemDecoder) (Debug.log "got here" value) of
                        Ok items ->
                            items |> List.filter (\i -> not <| String.isEmpty i.mediaUrl)

                        Err err ->
                            Debug.log err []
            in
            ( { model
                | items = newItems
                , categories = categories newItems
                , animState = Steady
              }
            , Cmd.none
            )

        ChangeQuery value ->
            ( { model | query = value }, Cmd.none )

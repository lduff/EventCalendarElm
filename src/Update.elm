module Update exposing (update)

import Date.Extra.Duration exposing (..)
import Material
import Models exposing (Model)
import Msgs exposing (Msg(..))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetDate d ->
            ( { model | currentDate = Just d }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model

        AdjustCalendar days ->
            ( { model
                | start = add Day days model.start
                , end = add Day days model.end
              }
            , Cmd.none
            )

        ToggleCategory toggleCategory ->
            ( { model
                | categories =
                    model.categories
                        |> List.map
                            (\c ->
                                if c.name == toggleCategory then
                                    { c | selected = not c.selected }
                                else
                                    c
                            )
              }
            , Cmd.none
            )

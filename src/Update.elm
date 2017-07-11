module Update exposing (update)

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

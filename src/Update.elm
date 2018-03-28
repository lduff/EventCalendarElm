module Update exposing (update)

import Date.Extra.Duration exposing (..)
import Date.Extra.Format as Format
import Json.Decode exposing (decodeValue, list)
import Models exposing (AnimState(..), Model, beginningOfWeek, calendarItemDecoder, categoriesFromSources, endOfWeek, sourceDecoder, stringToCalendarView, stringToChannelView)
import Msgs exposing (Msg(..))
import Ports exposing (..)


saveSettings : Model -> Cmd Msg
saveSettings model =
    saveUserSettings <|
        Models.UserSettings
            (model.categories
                |> List.filter (not << .selected)
                |> List.map .name
            )
            (Models.channelViewToString model.selectedChannel)
            (Models.calendarViewToString model.calendarView)


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

                newModel =
                    { model | categories = updatedCategories }
            in
            ( newModel, saveSettings model )

        Search ->
            ( { model | animState = Loading }
            , search <| Models.SearchQuery model.query (Format.isoString model.start) (Format.isoString model.end)
            )

        SearchResults value ->
            let
                newItems =
                    case decodeValue (list calendarItemDecoder) value of
                        Ok items ->
                            items
                                |> List.map
                                    (\i ->
                                        { i
                                            | allCategories =
                                                items
                                                    |> List.filter (\i2 -> i2.url == i.url)
                                                    |> List.map (\i2 -> i2.category)
                                        }
                                    )

                        Err err ->
                            Debug.log err []
            in
            ( { model
                | items = newItems
                , animState = Steady
              }
            , Cmd.none
            )

        ChangeQuery value ->
            ( { model | query = value }, Cmd.none )

        GetSources ->
            ( { model | animState = Loading }
            , getSources ()
            )

        Sources value ->
            let
                newCategories =
                    case decodeValue (list sourceDecoder) value of
                        Ok sources ->
                            sources |> categoriesFromSources

                        Err err ->
                            Debug.log err []
            in
            ( { model | categories = newCategories }, getUserSettings () )

        SelectChannel channel ->
            let
                newModel =
                    { model | selectedChannel = channel }
            in
            ( newModel, saveSettings newModel )

        SelectCalendarView view ->
            let
                newModel =
                    { model | calendarView = view }
            in
            ( newModel, saveSettings newModel )

        UserSettingsResults value ->
            let
                userSettings =
                    case decodeValue Models.userSettingsDecoder value of
                        Ok settings ->
                            settings

                        Err err ->
                            Debug.log err <| Models.UserSettings [] "retail" "by site"
            in
            ( { model
                | calendarView = stringToCalendarView userSettings.selectedView
                , selectedChannel = stringToChannelView userSettings.selectedChannel
                , categories =
                    model.categories
                        |> List.map
                            (\c ->
                                if List.member c.name userSettings.filteredCategories then
                                    { c | selected = False }
                                else
                                    c
                            )
              }
            , Cmd.none
            )

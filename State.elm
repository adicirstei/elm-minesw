module State exposing (initialState, update, subscriptions)

import Types exposing (..)
import StartScreen.State as SS


initialState =
    ( StartScreen, Cmd.none )


subscriptions model =
    case model of
        StartScreen ->
            Sub.none

        Playing g ->
            Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! []



--
-- update : Msg -> AppState -> ( AppState, Cmd Msg )
-- update msg state =
--     case state of
--         StartScreen ->
--             case msg of
--                 StartBeginner ->
--                     Playing MineSweeper.startBeginner ! []
--
--                 StartIntermediate ->
--                     Playing MineSweeper.startIntermediate ! []
--
--                 StartAdvanced ->
--                     Playing MineSweeper.startAdvanced ! []
--
--                 _ ->
--                     state ! []
--
--         Playing game ->
--             case msg of
--                 Tick time ->
--                     Playing (MineSweeper.tick game time) ! []
--
--                 Game action ->
--                     Playing (MineSweeper.update action game) ! []
--
--                 _ ->
--                     state ! []

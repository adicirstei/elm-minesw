module State exposing (initialState, update, subscriptions)

import Types exposing (..)
import Game.State
import StartScreen.Types as SST


initialState : ( Model, Cmd Msg )
initialState =
    ( StartScreen, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        StartScreen ->
            Sub.none

        Game g ->
            Game.State.subscriptions g
                |> Sub.map GameMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        StartScreen ->
            case msg of
                StartScreenMsg ss ->
                    processStartScreenMsg ss ! []

                _ ->
                    model ! []

        Game game ->
            case msg of
                GameMsg gm ->
                    processGameMessages gm game ! []

                _ ->
                    model ! []


processGameMessages msg game =
    Game (Game.State.update msg game)


processStartScreenMsg : SST.Msg -> Types.Model
processStartScreenMsg msg =
    case msg of
        SST.StartBeginner ->
            Game (Game.State.init 8 8 10)

        SST.StartIntermediate ->
            Game (Game.State.init 16 16 40)

        SST.StartAdvanced ->
            Game (Game.State.init 30 20 99)

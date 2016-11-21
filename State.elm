module State exposing (initialState, update, subscriptions)

import Types exposing (..)
import Game.State
import StartScreen.Types as SST
import Game.Types


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
    case msg of
        StartScreenMsg ss ->
            processStartScreenMsg ss ! []

        GoMsg GoRestart d ->
            initGame d ! []

        GameMsg gm ->
            processGameMessages gm model


initGame : Game.Types.Dificulty -> Types.Model
initGame d =
    Game <|
        case d of
            Begginer ->
                Game.State.init 9 7 10 d

            Intermediate ->
                Game.State.init 20 12 40 d

            Advanced ->
                Game.State.init 20 12 40 d


processGameMessages : Msg -> Model -> ( Model, Cmd msg )
processGameMessages msg model =
    case model of
        Game Playing gd ->
            Game Playing gd ! []

        _ ->
            model ! []


processStartScreenMsg : SST.Msg -> Types.Model
processStartScreenMsg msg =
    case msg of
        SST.StartBeginner ->
            initGame Begginer

        SST.StartIntermediate ->
            initGame Intermediate

        SST.StartAdvanced ->
            initGame Advanced

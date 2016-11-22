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
            processStartScreenMsg ss

        GameMsg gm ->
            processGameMessages gm model


initGame : Game.Types.Dificulty -> Types.Model
initGame d =
    Game <|
        case d of
            Game.Types.Beginner ->
                Game.State.init 9 7 10 d

            Game.Types.Intermediate ->
                Game.State.init 20 12 40 d

            Game.Types.Advanced ->
                Game.State.init 20 12 40 d


processGameOverMsg : Game.Types.GoMsg -> ( Model, Cmd msg )
processGameOverMsg (Game.Types.GoRestart d) =
    initGame d ! []


processGameMessages : Game.Types.Msg -> Model -> ( Model, Cmd Msg )
processGameMessages msg model =
    case ( msg, model ) of
        ( Game.Types.GoMsg (Game.Types.GoRestart d), _ ) ->
            initGame d ! []

        ( _, Game game ) ->
            Game.State.update msg game
                |> (\( g, cmd ) -> ( Game g, Cmd.map GameMsg cmd ))

        _ ->
            model ! []


processStartScreenMsg : SST.Msg -> ( Model, Cmd msg )
processStartScreenMsg msg =
    case msg of
        SST.StartBeginner ->
            initGame Game.Types.Beginner ! []

        SST.StartIntermediate ->
            initGame Game.Types.Intermediate ! []

        SST.StartAdvanced ->
            initGame Game.Types.Advanced ! []

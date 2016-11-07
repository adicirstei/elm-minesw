module Main exposing (..)

import Html.App
import Html exposing (..)
import Html.Events exposing (..)
import MineSweeper exposing (..)
import Time exposing (Time, every, second)


type Msg
    = StartBeginner
    | StartIntermediate
    | StartAdvanced
    | Tick Time
    | Game MineSweeper.Msg


type AppState
    = StartScreen
    | Playing MineSweeper.Model


main =
    Html.App.program
        { init = StartScreen ! []
        , subscriptions = subs
        , view = view
        , update = update
        }


subs model =
    case model of
        StartScreen ->
            Sub.none

        Playing g ->
            every second Tick


view : AppState -> Html Msg
view model =
    case model of
        StartScreen ->
            renderStartScreen

        Playing game ->
            MineSweeper.view Game game


renderStartScreen =
    body []
        [ button [ onClick StartBeginner ] [ text "Beginner" ]
        , button [ onClick StartIntermediate ] [ text "Intermediate" ]
        , button [ onClick StartAdvanced ] [ text "Advanced" ]
        ]


update : Msg -> AppState -> ( AppState, Cmd Msg )
update msg state =
    case state of
        StartScreen ->
            case msg of
                StartBeginner ->
                    Playing MineSweeper.startBeginner ! []

                StartIntermediate ->
                    Playing MineSweeper.startIntermediate ! []

                StartAdvanced ->
                    Playing MineSweeper.startAdvanced ! []

                _ ->
                    state ! []

        Playing game ->
            case msg of
                Tick time ->
                    Playing (MineSweeper.tick game time) ! []

                Game action ->
                    Playing (MineSweeper.update action game) ! []

                _ ->
                    state ! []

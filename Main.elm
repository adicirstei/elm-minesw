module Main exposing (..)

import Html.App
import Html exposing (..)
import Html.Events exposing (..)
import MineSweeper exposing (..)


type Msg gameaction
    = StartBeginner
    | StartIntermediate
    | StartAdvanced
    | Play gameaction


type AppState game
    = StartScreen
    | Playing game


main =
    Html.App.beginnerProgram
        { model = StartScreen
        , view = view
        , update = update
        }


view : AppState MineSweeper.Model -> Html (Msg MineSweeper.Msg)
view model =
    case model of
        StartScreen ->
            renderStartScreen

        Playing game ->
            MineSweeper.view Play game


renderStartScreen =
    body []
        [ button [ onClick StartBeginner ] [ text "Beginner" ]
        , button [ onClick StartIntermediate ] [ text "Intermediate" ]
        , button [ onClick StartAdvanced ] [ text "Advanced" ]
        ]


update : Msg (MineSweeper.Msg) -> AppState MineSweeper.Model -> AppState MineSweeper.Model
update msg state =
    case msg of
        StartBeginner ->
            Playing MineSweeper.startBeginner

        StartIntermediate ->
            Playing MineSweeper.startIntermediate

        StartAdvanced ->
            Playing MineSweeper.startAdvanced

        Play action ->
            case state of
                StartScreen ->
                    Debug.log "Impossible state" (state)

                Playing game ->
                    Playing (MineSweeper.update action game)

module Types exposing (..)

import StartScreen.Types
import Game.Types


type Model
    = StartScreen
    | Game Game.Types.Model


type Msg
    = StartScreenMsg StartScreen.Types.Msg
    | GameMsg Game.Types.Msg

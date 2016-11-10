module Types exposing (..)

import StartScreen.Types
import Game.Types as Game


type Model
    = StartScreen
    | Playing Game.Model

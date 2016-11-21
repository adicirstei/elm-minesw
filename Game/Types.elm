module Game.Types exposing (..)

import Dict exposing (Dict)
import Time exposing (Time)


type alias Cell =
    ( Int, Int )


type CellState
    = Hidden
    | Visible Int
    | Flagged
    | Mine


type PlayMsg
    = CellClicked Cell
    | CellFlagged Cell
    | Tick Time
    | SwitchMode


type GoMsg
    = GoRestart Dificulty


type Msg
    = PlayMsg PlayMsg
    | GoMsg GoMsg


type Model
    = Playing GameData
    | GameWon GameData
    | GameLost GameData


type Dificulty
    = Beginner
    | Intermediate
    | Advanced


type alias GameData =
    { grid : Dict Cell CellState
    , cols : Int
    , rows : Int
    , score : Int
    , mines : List Cell
    , minesCount : Int
    , time : Int
    , startTime : Time
    , cellMsg : Cell -> PlayMsg
    , dificulty : Dificulty
    }

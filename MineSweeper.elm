module MineSweeper exposing (Model, Msg, view, update, startBeginner, startIntermediate, startAdvanced)

import Html exposing (..)
import Dict exposing (Dict)


type alias Cell =
    ( Int, Int )


type CellState
    = Hidden
    | Visible Int
    | Empty


type Msg
    = CellClicked Cell
    | HintClicked


type alias Model =
    { grid : Dict Cell CellState
    , cols : Int
    , rows : Int
    , score : Int
    , mines : List Cell
    }


startBeginner =
    Model Dict.empty 10 10 0 [ ( 3, 3 ), ( 7, 2 ) ]


startIntermediate =
    Model Dict.empty 20 15 0 [ ( 3, 3 ), ( 7, 2 ) ]


startAdvanced =
    Model Dict.empty 30 20 0 [ ( 3, 3 ), ( 7, 2 ) ]


view x =
    div [] []


update msg model =
    model

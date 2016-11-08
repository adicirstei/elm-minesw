module MineSweeper exposing (Model, Msg, view, update, startBeginner, startIntermediate, startAdvanced, tick)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Time exposing (Time)
import String exposing (padLeft)
import Random
import Set


type alias Cell =
    ( Int, Int )


type CellState
    = Hidden
    | Visible Int
    | Empty
    | Mine


type Msg
    = CellClicked Cell
    | CellFlagged Cell
    | GameLost


type alias Model =
    { grid : Dict Cell CellState
    , cols : Int
    , rows : Int
    , score : Int
    , mines : List Cell
    , minesCount : Int
    , time : Int
    , startTime : Time
    }


startBeginner : Model
startBeginner =
    Model Dict.empty 8 8 0 [] 10 0 0


startIntermediate : Model
startIntermediate =
    Model Dict.empty 16 16 0 [] 50 0 0


startAdvanced : Model
startAdvanced =
    Model Dict.empty 30 20 0 [] 100 0 0


view msg model =
    div [ style [ ( "font-family", "monospace" ), ( "font-size", "24px" ) ] ]
        [ div
            [ class "header"
            , style [ ( "width", (toString (model.cols * 31)) ++ "px" ) ]
            ]
            [ div
                [ style
                    [ ( "float", "left" )
                    , ( "color", "red" )
                    , ( "width", "33.3333%" )
                    ]
                ]
                [ text (model.minesCount |> format) ]
            , div
                [ style
                    [ ( "color", "black" )
                    , ( "background", "yellow" )
                    , ( "width", "48px" )
                    , ( "margin", "0 auto" )
                    ]
                ]
                [ text ":-)" ]
            , div
                [ style
                    [ ( "float", "right" )
                    , ( "color", "red" )
                    , ( "width", "33.3333%" )
                    , ( "text-align", "right" )
                    ]
                ]
                [ text (format model.time) ]
            ]
        , div
            [ class "grid"
            , style
                [ ( "background-color", "white" )
                , ( "font-size", "16px" )
                , ( "clear", "both" )
                ]
            ]
            (renderGrid msg model)
        ]


renderGrid msg model =
    ([0..model.rows] |> List.map (renderRow msg model))


renderRow msg model row =
    div [ style [ ( "height", "28px" ) ] ]
        ([0..model.cols] |> List.map (renderCell msg model row))


renderCell msg model row col =
    case Dict.get ( row, col ) model.grid |> Maybe.withDefault Hidden of
        Hidden ->
            div
                [ style
                    [ ( "width", "24px" )
                    , ( "height", "24px" )
                    , ( "margin", "1px" )
                    , ( "display", "inline-block" )
                    , ( "background-color", "gray" )
                    , ( "border", "1px solid #333" )
                    ]
                , onClick (msg (CellClicked ( row, col )))
                ]
                []

        Visible mines ->
            div
                [ style
                    [ ( "width", "24px" )
                    , ( "height", "24px" )
                    , ( "margin", "1px" )
                    , ( "display", "inline-block" )
                    , ( "background-color", "lightgray" )
                    , ( "vertical-align", "top" )
                    , ( "border", "1px solid lightgray" )
                    , ( "padding-top", "5px" )
                    , ( "color", color mines )
                    , ( "font-weight", "bold" )
                    , ( "text-align", "center" )
                    ]
                ]
                [ if mines == 0 then
                    text ""
                  else
                    text (toString mines)
                ]

        Mine ->
            div
                [ style
                    [ ( "width", "24px" )
                    , ( "height", "24px" )
                    , ( "margin", "1px" )
                    , ( "display", "inline-block" )
                    , ( "background-color", "red" )
                    , ( "vertical-align", "top" )
                    , ( "border", "1px solid darkred" )
                    , ( "padding-top", "5px" )
                    , ( "font-weight", "bold" )
                    , ( "text-align", "center" )
                    ]
                ]
                [ text "*"
                ]

        _ ->
            div
                [ style
                    [ ( "width", "24px" )
                    , ( "height", "24px" )
                    , ( "margin", "1px" )
                    , ( "display", "inline-block" )
                    , ( "background-color", "gray" )
                    ]
                ]
                []


format =
    toString >> (padLeft 3 '0')


color m =
    case m of
        0 ->
            "lightgray"

        1 ->
            "blue"

        2 ->
            "green"

        3 ->
            "red"

        4 ->
            "darkblue"

        5 ->
            "darkred"

        6 ->
            "teal"

        _ ->
            "black"


tick model time =
    if model.grid == Dict.empty then
        { model | startTime = time }
            |> Debug.log "tick model"
    else
        { model | time = model.time + 1 }


getCellState model cell =
    if List.member cell model.mines then
        Mine
    else
        Visible 8


unique =
    List.foldl
        (\x acc ->
            if List.member x acc then
                acc
            else
                x :: acc
        )
        []


randomMines cell model seed =
    let
        ( row, col ) =
            cell

        cellIdx =
            (row * model.cols + col)
                |> Debug.log "cellIdx"

        gen =
            Random.list (model.rows * model.cols) (Random.int 0 (model.rows * model.cols - 1))
                |> Random.map
                    (\idxs ->
                        idxs
                            |> Debug.log "idxs"
                            |> List.filter ((/=) cellIdx)
                            |> unique
                            |> List.take model.minesCount
                            |> List.map (\idx -> ( idx // model.cols, idx % model.cols ))
                    )
    in
        Random.step gen seed |> Debug.log "gen list" |> fst


initMines model cell =
    let
        seed =
            Random.initialSeed (floor model.startTime)
    in
        case model.mines of
            [] ->
                { model | mines = randomMines cell model seed }
                    |> Debug.log "mines generated"

            _ ->
                model


updateGrid cell model =
    { model | grid = Dict.insert cell (getCellState model cell) model.grid }


update : Msg -> Model -> Model
update msg model =
    case msg of
        CellClicked cell ->
            cell
                |> initMines model
                |> updateGrid cell

        _ ->
            model

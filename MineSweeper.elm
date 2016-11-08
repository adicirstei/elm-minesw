module MineSweeper exposing (Model, Msg, view, update, startBeginner, startIntermediate, startAdvanced, tick)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Time exposing (Time)
import String exposing (padLeft)
import Random
import List.Extras as List


type alias Cell =
    ( Int, Int )


type CellState
    = Hidden
    | Visible Int
    | Flagged
    | Mine


type Msg
    = CellClicked Cell
    | CellFlagged Cell
    | Restart


type alias GameModel =
    { grid : Dict Cell CellState
    , cols : Int
    , rows : Int
    , score : Int
    , mines : List Cell
    , minesCount : Int
    , time : Int
    , startTime : Time
    }


type Model
    = Playing GameModel
    | GameWon GameModel
    | GameLost GameModel


initDict rows cols =
    [0..rows - 1]
        |> List.map (\r -> [0..cols - 1] |> List.map (\c -> ( ( r, c ), Hidden )))
        |> List.concat
        |> Dict.fromList


startBeginner : Model
startBeginner =
    Playing (GameModel (initDict 8 8) 8 8 0 [] 10 0 0)


startIntermediate : Model
startIntermediate =
    Playing (GameModel (initDict 16 16) 16 16 0 [] 50 0 0)


startAdvanced : Model
startAdvanced =
    Playing (GameModel (initDict 30 20) 30 20 0 [] 100 0 0)


view msg model =
    case model of
        Playing gm ->
            drawBoard msg gm

        GameWon gm ->
            div []
                [ drawBoard msg gm
                , div
                    [ style
                        [ ( "position", "absolute" )
                        , ( "top", "100px" )
                        , ( "left", "50px" )
                        , ( "font-size", "64px" )
                        , ( "padding", "24px" )
                        , ( "background", "yellow" )
                        ]
                    ]
                    [ text "Victory!" ]
                ]

        GameLost gm ->
            div []
                [ drawBoard msg gm
                , div
                    [ style
                        [ ( "position", "absolute" )
                        , ( "top", "100px" )
                        , ( "left", "50px" )
                        , ( "font-size", "64px" )
                        , ( "padding", "24px" )
                        , ( "background", "yellow" )
                        ]
                    ]
                    [ text "You lost!" ]
                ]


drawBoard msg gm =
    div [ style [ ( "font-family", "monospace" ), ( "font-size", "24px" ) ] ]
        [ div
            [ class "header"
            , style [ ( "width", (toString (gm.cols * 31)) ++ "px" ) ]
            ]
            [ div
                [ style
                    [ ( "float", "left" )
                    , ( "color", "red" )
                    , ( "width", "33.3333%" )
                    ]
                ]
                [ text (gm.minesCount |> format) ]
            , div
                [ style
                    [ ( "color", "black" )
                    , ( "background", "yellow" )
                    , ( "width", "48px" )
                    , ( "margin", "0 auto" )
                    ]
                , onClick (msg Restart)
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
                [ text (format gm.time) ]
            ]
        , div
            [ class "grid"
            , style
                [ ( "background-color", "white" )
                , ( "font-size", "16px" )
                , ( "clear", "both" )
                ]
            ]
            (renderGrid msg gm)
        ]


renderGrid msg model =
    ([0..model.rows - 1] |> List.map (renderRow msg model))


renderRow msg model row =
    div [ style [ ( "height", "28px" ) ] ]
        ([0..model.cols - 1] |> List.map (renderCell msg model row))


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
    case model of
        Playing gm ->
            if gm.mines == [] then
                Playing { gm | startTime = time }
            else
                Playing { gm | time = gm.time + 1 }

        _ ->
            model


getCellState model ( r, c ) =
    if List.member ( r, c ) model.mines then
        Mine
    else
        let
            ns =
                [ ( r - 1, c - 1 ), ( r - 1, c ), ( r - 1, c + 1 ), ( r, c - 1 ), ( r, c + 1 ), ( r + 1, c - 1 ), ( r + 1, c ), ( r + 1, c + 1 ) ]
                    |> List.filter (\( r, c ) -> r >= 0 && r < model.rows && c >= 0 && c < model.cols)

            ms =
                model.mines
                    |> List.filter (\m -> List.member m ns)
                    |> List.length
        in
            Visible ms


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
                            |> List.unique
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


wonLostContinue gm =
    let
        flagsOrHidden =
            Dict.filter (\_ v -> v == Flagged || v == Hidden) gm.grid
                |> Debug.log "flags or hidden"

        mines =
            Dict.filter (\_ v -> v == Mine) gm.grid
    in
        if Dict.size mines > 0 then
            GameLost gm
        else if Dict.size flagsOrHidden == List.length gm.mines then
            GameWon gm
        else
            Playing gm


update : Msg -> Model -> Model
update msg model =
    case model of
        Playing gm ->
            case msg of
                CellClicked cell ->
                    cell
                        |> initMines gm
                        |> updateGrid cell
                        |> wonLostContinue

                Restart ->
                    case gm.cols of
                        8 ->
                            startBeginner

                        16 ->
                            startIntermediate

                        _ ->
                            startAdvanced

                _ ->
                    Playing gm

        GameLost gm ->
            case msg of
                Restart ->
                    case gm.cols of
                        8 ->
                            startBeginner

                        16 ->
                            startIntermediate

                        _ ->
                            startAdvanced

                _ ->
                    GameLost gm

        GameWon gm ->
            case msg of
                Restart ->
                    case gm.cols of
                        8 ->
                            startBeginner

                        16 ->
                            startIntermediate

                        _ ->
                            startAdvanced

                _ ->
                    GameWon gm

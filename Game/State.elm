module Game.State exposing (init, update, subscriptions)

import Time exposing (every, second)
import Game.Types exposing (..)
import Dict
import Random
import List.Extras as List
import Task


winCmd : GameData -> Cmd Msg
winCmd data =
    Task.succeed ()
        |> Task.perform (\_ -> Won data)


lostCmd : GameData -> Cmd Msg
lostCmd data =
    Task.succeed ()
        |> Task.perform (\_ -> Lost data)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Playing _ ->
            every second Tick
                |> Sub.map PlayMsg

        _ ->
            Sub.none


init : Int -> Int -> Int -> Dificulty -> Model
init cols rows mines d =
    GameData (initDict cols rows) cols rows 0 [] mines 0 0 CellClicked d Dig
        |> Playing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( PlayMsg m, Playing gd ) ->
            playUpdate m gd
                |> (\( gd, cmd ) -> ( Playing gd, cmd ))

        ( Won gd, _ ) ->
            GameWon gd ! []

        ( Lost gd, _ ) ->
            GameLost gd ! []

        _ ->
            model ! []


playUpdate : PlayMsg -> GameData -> ( GameData, Cmd Msg )
playUpdate msg data =
    case msg of
        Tick time ->
            if data.mines == [] then
                { data | startTime = time } ! []
            else
                { data | time = data.time + 1 } ! []

        CellClicked cell ->
            cell
                |> initMines data
                |> updateGrid cell
                |> wonLostContinue

        SwitchMode ->
            let
                ( nextMode, cb ) =
                    if data.mode == Dig then
                        ( Flag, CellFlagged )
                    else
                        ( Dig, CellClicked )
            in
                { data | cellMsg = cb, mode = nextMode } ! []

        CellFlagged cell ->
            let
                c =
                    Dict.get cell data.grid |> Maybe.withDefault Hidden

                ( newState, counter ) =
                    if c == Flagged then
                        ( Hidden, 1 )
                    else if data.minesCount == 0 then
                        ( c, 0 )
                    else
                        ( Flagged, -1 )
            in
                { data
                    | grid = Dict.insert cell newState data.grid
                    , minesCount = data.minesCount + counter
                }
                    ! []


initMines model cell =
    let
        seed =
            Random.initialSeed (floor model.startTime)
    in
        case model.mines of
            [] ->
                { model | mines = randomMines cell model seed }

            _ ->
                model


randomMines cell model seed =
    let
        ( row, col ) =
            cell

        cellIdx =
            (row * model.cols + col)

        gen =
            Random.list (model.rows * model.cols) (Random.int 0 (model.rows * model.cols - 1))
                |> Random.map
                    (\idxs ->
                        idxs
                            |> List.filter ((/=) cellIdx)
                            |> List.unique
                            |> List.take model.minesCount
                            |> List.map (\idx -> ( idx // model.cols, idx % model.cols ))
                    )
    in
        Random.step gen seed |> Tuple.first


getNeighbours model cell =
    let
        ( r, c ) =
            cell
    in
        [ ( r - 1, c - 1 ), ( r - 1, c ), ( r - 1, c + 1 ), ( r, c - 1 ), ( r, c + 1 ), ( r + 1, c - 1 ), ( r + 1, c ), ( r + 1, c + 1 ) ]
            |> List.filter (\( r, c ) -> r >= 0 && r < model.rows && c >= 0 && c < model.cols)


getCellState model cell =
    if List.member cell model.mines then
        Mine
    else
        let
            ns =
                getNeighbours model cell

            ms =
                model.mines
                    |> List.filter (\m -> List.member m ns)
                    |> List.length
        in
            Visible ms


updateGrid cell model =
    let
        cellState =
            getCellState model cell
    in
        if cellState == Visible 0 then
            let
                ns =
                    getNeighbours model cell
                        |> List.filter
                            (\c ->
                                Dict.filter (\k v -> k == c && v == Hidden) model.grid
                                    |> Dict.size
                                    |> ((<) 0)
                            )
            in
                List.foldr updateGrid { model | grid = Dict.insert cell cellState model.grid } ns
        else
            { model | grid = Dict.insert cell cellState model.grid }


wonLostContinue gm =
    let
        flagsOrHidden =
            Dict.filter (\_ v -> v == Flagged || v == Hidden) gm.grid

        mines =
            Dict.filter (\_ v -> v == Mine) gm.grid
    in
        if Dict.size mines > 0 then
            ( gm, lostCmd gm )
        else if Dict.size flagsOrHidden == List.length gm.mines then
            ( gm, winCmd gm )
        else
            gm ! []


initDict cols rows =
    List.range 0 (rows - 1)
        |> List.map (\r -> List.range 0 (cols - 1) |> List.map (\c -> ( ( r, c ), Hidden )))
        |> List.concat
        |> Dict.fromList

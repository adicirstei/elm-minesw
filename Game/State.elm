module Game.State exposing (init, update, subscriptions)

import Time exposing (every, second)
import Game.Types exposing (..)
import Dict
import Random
import List.Extras as List
import Task


restart : Dificulty -> Cmd GoMsg
restart d =
    Task.succeed ()
        |> Task.perform (\_ -> GoRestart d)


subscriptions : Model -> Sub Msg
subscriptions model =
    every second Tick
        |> Sub.map PlayMsg


init : Int -> Int -> Int -> Dificulty -> Model
init cols rows mines d =
    GameData (initDict cols rows) cols rows 0 [] mines 0 0 CellClicked d
        |> Playing



--
-- update : Msg -> Model -> Model
-- update msg model =
--     case msg of
--         PlayMsg m ->
--             playUpdate m model
--
--         GoMsg m ->
--             goUpdate m model
--
--


goUpdate : GoMsg -> Model -> Model
goUpdate msg model =
    case model of
        GameWon g ->
            init g.cols g.rows g.minesCount

        GameLost g ->
            init g.cols g.rows g.minesCount

        _ ->
            model


playUpdate : PlayMsg -> GameData -> GameData
playUpdate msg model =
    case model of
        Playing g ->
            case msg of
                Tick time ->
                    if g.mines == [] then
                        Playing { g | startTime = time }
                    else
                        Playing { g | time = g.time + 1 }

                CellClicked cell ->
                    cell
                        |> initMines g
                        |> updateGrid cell
                        |> wonLostContinue

                SwitchMode ->
                    Playing { g | cellMsg = flip g.cellMsg }

                CellFlagged cell ->
                    Playing { g | grid = Dict.insert cell Flagged g.grid }

        _ ->
            model


flip fn =
    if fn == CellClicked then
        CellFlagged
    else
        CellClicked


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
            GameLost gm
        else if Dict.size flagsOrHidden == List.length gm.mines then
            GameWon gm
        else
            Playing gm


initDict cols rows =
    List.range 0 (rows - 1)
        |> List.map (\r -> List.range 0 (cols - 1) |> List.map (\c -> ( ( r, c ), Hidden )))
        |> List.concat
        |> Dict.fromList

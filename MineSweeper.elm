module MineSweeper exposing (Model, Msg, view, update, startBeginner, startIntermediate, startAdvanced, tick)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Time exposing (Time)


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
    , time : Int
    }


startBeginner : Model
startBeginner =
    Model Dict.empty 10 10 0 [ ( 3, 3 ), ( 7, 2 ) ] 0


startIntermediate : Model
startIntermediate =
    Model Dict.empty 20 15 0 [ ( 3, 3 ), ( 7, 2 ) ] 0


startAdvanced : Model
startAdvanced =
    Model Dict.empty 30 20 0 [ ( 3, 3 ), ( 7, 2 ) ] 0


view msg model =
    div []
        [ h2 []
            [ text ("Mines: " ++ (toString model.score))
            , text ("Time: " ++ toString model.time)
            ]
        , div
            [ class "grid"
            , style [ ( "background-color", "white" ) ]
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
                    ]
                ]
                [ if mines == 0 then
                    text ""
                  else
                    text (toString mines)
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


tick model time =
    if model.grid == Dict.empty then
        model
    else
        { model | time = model.time + 1 }


getCellState model cell =
    Visible 3


update : Msg -> Model -> Model
update msg model =
    case msg of
        CellClicked cell ->
            { model | grid = Dict.insert cell (getCellState model cell) model.grid }

        _ ->
            model

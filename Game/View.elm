module Game.View exposing (root)

import Html exposing (Html)
import String exposing (padLeft)
import Dict exposing (Dict)
import Game.Types exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)


root : Model -> Html Msg
root model =
    svg [ viewBox "0 0 100 111", width "60%" ]
        (case model of
            Playing gm ->
                [ drawBoard gm
                    |> Html.map PlayMsg
                ]

            GameWon gm ->
                [ drawBoard gm
                    |> Html.map (\_ -> GoMsg GoRestart)
                , g
                    []
                    [ rect [ x "20", y "40", width "60", height "20", fill "yellow" ] []
                    , text_
                        [ fill "red"
                        , fontSize "14"
                        , x "50"
                        , y "50"
                        , textAnchor "middle"
                        , alignmentBaseline "central"
                        ]
                        [ text "Victory!" ]
                    ]
                ]

            GameLost gm ->
                [ drawBoard gm |> Html.map (\_ -> GoMsg GoRestart)
                , g
                    []
                    [ rect [ x "20", y "40", width "60", height "20", fill "yellow" ] []
                    , text_
                        [ fill "red"
                        , fontSize "14"
                        , x "50"
                        , y "50"
                        , textAnchor "middle"
                        , alignmentBaseline "central"
                        ]
                        [ text "You lost!" ]
                    ]
                ]
        )


drawBoard : GameData -> Html PlayMsg
drawBoard gm =
    g
        [ x "0", y "0", width "100", height "10" ]
        [ g
            []
            [ g
                []
                [ rect [ x "0", y "0", width "20", height "10", fill "black" ] []
                , text_
                    [ fill "red"
                    , fontSize "7"
                    , x "10"
                    , y "5"
                    , textAnchor "middle"
                    , alignmentBaseline "central"
                    ]
                    [ text (gm.minesCount |> format) ]
                ]
            , g
                [ onClick Restart
                ]
                [ rect
                    [ x "40"
                    , y "0"
                    , width "20"
                    , height "10"
                    , fill "yellow"
                    ]
                    []
                , text_
                    [ fill "red"
                    , fontSize "7"
                    , x "50"
                    , y "5"
                    , textAnchor "middle"
                    , alignmentBaseline "central"
                    ]
                    [ text ":-)" ]
                ]
            , g
                []
                [ rect [ x "80", y "0", width "20", height "10", fill "black" ] []
                , text_
                    [ fill "red"
                    , fontSize "7"
                    , x "90"
                    , y "5"
                    , textAnchor "middle"
                    , alignmentBaseline "central"
                    ]
                    [ text (format gm.time) ]
                ]
            ]
        , g
            []
            (renderGrid gm)
        ]


renderGrid model =
    (List.range 0 (model.rows - 1) |> List.map (renderRow model))


renderRow model row =
    g
        []
        (List.range 0 (model.cols - 1) |> List.map (renderCell model row))


renderCell model row col =
    let
        cellSize =
            100.0 / (toFloat model.cols)
    in
        case Dict.get ( row, col ) model.grid |> Maybe.withDefault Hidden of
            Hidden ->
                rect
                    [ onClick (CellClicked ( row, col ))
                    , width (toString cellSize)
                    , height (toString cellSize)
                    , fill "grey"
                    , stroke "white"
                    , strokeWidth "0.1"
                    , x <| toString (toFloat col * cellSize)
                    , y <| toString (toFloat row * cellSize + 11.0)
                    ]
                    []

            Visible mines ->
                g
                    []
                    [ rect
                        [ width (toString cellSize)
                        , height (toString cellSize)
                        , fill "white"
                        , stroke "white"
                        , strokeWidth "0.1"
                        , x <| toString (toFloat col * cellSize)
                        , y <| toString (toFloat row * cellSize + 11.0)
                        ]
                        []
                    , text_
                        [ fill (color mines)
                        , fontSize (toString (cellSize * 0.5))
                        , textAnchor "middle"
                        , alignmentBaseline "central"
                        , x <| toString (toFloat col * cellSize + cellSize / 2.0)
                        , y <| toString (toFloat row * cellSize + cellSize / 2.0 + 11.0)
                        ]
                        [ if mines == 0 then
                            text ""
                          else
                            text (toString mines)
                        ]
                    ]

            Mine ->
                g []
                    [ rect
                        [ width (toString cellSize)
                        , height (toString cellSize)
                        , strokeWidth "0.1"
                        , fill "red"
                        , stroke "white"
                        , x <| toString (toFloat col * cellSize)
                        , y <| toString (toFloat row * cellSize + 11.0)
                        ]
                        []
                    , text_
                        [ fontSize (toString (cellSize * 0.5))
                        , textAnchor "middle"
                        , alignmentBaseline "central"
                        , x <| toString (toFloat col * cellSize + cellSize / 2.0)
                        , y <| toString (toFloat row * cellSize + cellSize / 2.0 + 11.0)
                        ]
                        [ text "*" ]
                    ]

            _ ->
                g
                    []
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

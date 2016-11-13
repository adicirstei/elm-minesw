module Game.View exposing (root)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import String exposing (padLeft)
import Dict exposing (Dict)
import Game.Types exposing (..)
import Html.App


root : Model -> Html Msg
root model =
    case model of
        Playing gm ->
            drawBoard gm
                |> Html.App.map PlayMsg

        GameWon gm ->
            div []
                [ drawBoard gm
                    |> Html.App.map (\_ -> GoMsg GoRestart)
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
                [ drawBoard gm |> Html.App.map (\_ -> GoMsg GoRestart)
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


drawBoard : GameData -> Html PlayMsg
drawBoard gm =
    div
        [ style
            [ ( "font-family", "monospace" )
            , ( "font-size", "24px" )
            , ( "width", "-webkit-min-content" )
            , ( "width", "-moz-min-content" )
            , ( "width", "min-content" )
            ]
        ]
        [ div
            [ class "header"
            , style [ ( "width", "100%" ), ( "background", "#222" ) ]
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
                , onClick Restart
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
            (renderGrid gm)
        ]


renderGrid model =
    ([0..model.rows - 1] |> List.map (renderRow model))


renderRow model row =
    div
        [ style
            [ ( "height", "28px" )
            , ( "clear", "both" )
            , ( "display", "inline-flex" )
            ]
        ]
        ([0..model.cols - 1] |> List.map (renderCell model row))


renderCell model row col =
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
                , onClick (CellClicked ( row, col ))
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

module View exposing (rootView)

import Html.App
import Html exposing (..)
import Html.Attributes exposing (..)
import Types exposing (..)
import StartScreen.View
import Game.View


rootView : Model -> Html Msg
rootView model =
    body []
        [ node "link"
            [ rel "stylesheet"
            , href "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.2.3/css/bulma.min.css"
            ]
            []
        , h1 [] [ text "Mine Sweeper" ]
        , case model of
            StartScreen ->
                startScreenView

            Game g ->
                gameView g
        ]


startScreenView =
    StartScreen.View.root ()
        |> Html.App.map StartScreenMsg


gameView model =
    Game.View.root model
        |> Html.App.map GameMsg

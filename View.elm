module View exposing (rootView)

import Html.App
import Html exposing (..)
import Types exposing (..)
import StartScreen.View


rootView : Model -> Html msg
rootView model =
    body []
        [ h1 [] [ text "Mine Sweeper" ]
        , case model of
            StartScreen ->
                StartScreen.View.root ()

            _ ->
                text "placeholder"
        ]

module StartScreen.View exposing (root)

import StartScreen.Types exposing (..)
import Html exposing (..)
import Html.Events exposing (..)


root : Model -> Html Msg
root model =
    div []
        [ button [ onClick StartBeginner ] [ text "Beginner" ]
        , button [ onClick StartIntermediate ] [ text "Intermediate" ]
        , button [ onClick StartAdvanced ] [ text "Advanced" ]
        ]

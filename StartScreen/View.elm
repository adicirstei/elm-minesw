module StartScreen.View exposing (root)

import StartScreen.Types exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (class)


root : Model -> Html Msg
root model =
    div []
        [ button [ class "button", onClick StartBeginner ] [ text "Beginner" ]
        , button [ class "button", onClick StartIntermediate ] [ text "Intermediate" ]
        , button [ class "button", onClick StartAdvanced ] [ text "Advanced" ]
        ]

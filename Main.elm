module Main exposing (main)

import Html
import State
import View


main =
    Html.program
        { init = State.initialState
        , update = State.update
        , view = View.rootView
        , subscriptions = State.subscriptions
        }

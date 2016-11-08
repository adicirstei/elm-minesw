module List.Extras exposing (..)


unique =
    List.foldl
        (\x acc ->
            if List.member x acc then
                acc
            else
                x :: acc
        )
        []

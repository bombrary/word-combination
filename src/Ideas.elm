module Ideas exposing (..)

import Array exposing (Array)
import Idea exposing (..)
import Random
import Random.Array as Random



-- Ideas


type alias Ideas =
    Array Idea


repeat : Int -> Idea -> Ideas
repeat n idea =
    Array.repeat n idea


compose : Ideas -> Ideas -> Ideas
compose xs ys =
    Array.foldr Array.append
        Array.empty
        (Array.map
            (\x ->
                Array.map
                    (\y ->
                        map2 (++) x y
                    )
                    ys
            )
            xs
        )


values : Ideas -> Array String
values ideas =
    Array.foldl
        (\e acc ->
            case e of
                Nothing ->
                    acc

                Just string ->
                    Array.push string acc
        )
        Array.empty
        ideas


toList : Ideas -> List Idea
toList ideas =
    Array.toList ideas


indexedMap : (Int -> Idea -> Idea) -> Ideas -> Ideas
indexedMap f ideas =
    Array.indexedMap f ideas


set : Int -> String -> Ideas -> Ideas
set i value ideas =
    Array.set i (Just value) ideas



-- Random Generator


choose : Int -> Array String -> Random.Generator Ideas
choose n origin =
    Random.shuffle origin
        |> Random.map
            (\arr ->
                Array.map Just (Array.slice 0 n arr)
            )

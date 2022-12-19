module Idea exposing (..)

import Maybe.Extra as Maybe



-- Idea


type alias Idea =
    Maybe String


map2 : (String -> String -> String) -> Idea -> Idea -> Idea
map2 f idea1 idea2 =
    Maybe.map2 f idea1 idea2


withDefault : String -> Idea -> String
withDefault default idea =
    Maybe.withDefault default idea

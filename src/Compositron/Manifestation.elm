module Compositron.Manifestation exposing (..)

import Helpers exposing (..)

import Compositron.View as View exposing ( View, Action (..) )

-- Manifestation


type Manifestation
    = Targeted
    | Cotargeted
    | Notargeted

targeted = (==) Targeted
           
view : Manifestation -> Map ( View node prototype )
view manifestation =
    case manifestation of
        Notargeted ->
            View.action Navigate_here
        Cotargeted ->
            View.action Navigate_here
        Targeted ->
            View.target
                >> View.action Focus_here
                                                                    


serialize : Manifestation -> String
serialize manifestation =
    case manifestation of
        Targeted -> "*"
        Cotargeted -> "~"
        Notargeted -> ""

deserialize : String -> Maybe Manifestation
deserialize str =
    case str of
        "*" -> Just Targeted
        "~" -> Just Cotargeted
        "" -> Just Notargeted
        _ -> Nothing

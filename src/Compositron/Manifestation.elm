module Compositron.Manifestation exposing (..)

import Helpers exposing (..)


-- Manifestation


type Manifestation
    = Targeted
    | Notargeted

targeted = (==) Targeted

serialize : Manifestation -> String
serialize manifestation =
    case manifestation of
        Targeted -> "*"
        Notargeted -> ""

deserialize : String -> Maybe Manifestation
deserialize str =
    case str of
        "*" -> Just Targeted
        "" -> Just Notargeted
        _ -> Nothing

{--
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
                                                                    
--}

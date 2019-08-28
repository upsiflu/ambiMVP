module Compositron.Manifestation exposing (..)

import Helpers exposing (..)

import Compositron.View as View exposing ( View, Action (..) )

-- Manifestation


type Manifestation
    = Targeted
    | Cotargeted
    | Notargeted

targeted = (==) Targeted
           
view : Manifestation -> Map ( View msg item signature data )
view manifestation =
    case manifestation of
        Notargeted ->
            View.add_action Navigate_here
                >> View.add_class "notargeted"
        Cotargeted ->
            View.add_action Navigate_here
                >> View.add_class "cotargeted"
        Targeted ->
            View.add_action Focus_here
                >> View.add_class "targeted"
        
                                                                    


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

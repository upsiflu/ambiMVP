module Compositron.Manifestation exposing (..)

import Helpers exposing (..)

import Compositron.View as View exposing ( View, Action (..) )

-- Manifestation


type alias Manifestation =
    { target : Bool }

target = always { target = True }
untarget = always { target = False }
targeted = (==) { target = True }

passive = untarget ()
active = target ()


view : Manifestation -> Map ( View msg item signature data )
view manifestation =
    if targeted manifestation
    then
        View.add_action Focus_here >> View.add_class "targeted"
    else
        View.add_action Navigate_here
                                                                    


serialize : Manifestation -> String
serialize manifestation =
    case targeted manifestation of
        True -> "▶"
        False -> " "

deserialize : String -> Maybe Manifestation
deserialize str =
    case str of
        "▶" -> Just active
        " " -> Just passive
        _ -> Nothing

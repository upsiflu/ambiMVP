module Compositron.Data exposing (..)



-- Data


type alias Data = Maybe String



-- read
    
    
is_empty = (==) Nothing



-- serialize


enstring : Data -> String
enstring = Maybe.withDefault ""

destring : String -> Data
destring string =
    case string of
        "" -> Nothing
        st -> Just st

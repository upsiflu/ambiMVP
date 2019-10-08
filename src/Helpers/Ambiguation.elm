module Helpers.Ambiguation exposing
  ( Ambiguation (..)
  , map
  , to_nonempty
  , from_nonempty
  , from_list
  , to_list
  )

{-|
# Definition
@docs Ambiguation

# Map
@docs map

# Convert (lossy!)
@docs from_nonempty
@docs to_nonempty
@docs from_list
@docs to_list
|-}

import Helpers.Nonempty as Nonempty exposing (Nonempty)


type alias Map a = a -> a

{-| Either one, or more members.-}
type Ambiguation a
    = Or a ( Ambiguation a )
    | Of a

{-| Map a function over all members.-}
map : Map a -> Map ( Ambiguation a )
map fu ambi =
    case ambi of
        Or x y -> Or ( fu x ) ( map fu y )
        Of x -> Of ( fu x )
                
{-| Mind: the distinction between "at least one" and "at least two" is lost.-}
to_nonempty : Ambiguation a -> Nonempty a
to_nonempty ambi =
    case ambi of
        Or x y -> ( x, to_list y )
        Of x -> ( x, [] )

{-| Lossless conversion, adding a distinction between "at least 2" and "at least 1".-}
from_nonempty : Nonempty a -> Ambiguation a
from_nonempty n =
    case n of
        ( head, [] ) -> Of head
        ( head, t::ail ) ->
            Or head ( from_nonempty ( t, ail ) )

{-| Lossy conversion to List.-}
to_list: Ambiguation a -> List a
to_list a =
    case a of
        Or x y -> x::( to_list y )
        Of x -> [x]

from_list : List a -> Maybe ( Ambiguation a )
from_list list =
    case list of
        [] -> Nothing
        x::y ->
            case from_list y of
                Nothing -> Just ( Of x )
                Just ambi -> Just ( Or x ambi )

module Helpers.Nonempty exposing
  ( Nonempty
  , singleton
  , map
  , fold
  , member
  , is_singleton
  , just_singleton
  , head
  , from_list
  , to_list
  )

{-|
# Definition
@docs Nonempty

# Create
@docs singleton

# Read
@docs member
@docs head
@docs just_singleton
@docs is_singleton

# Map, Fold
@docs map
@docs fold

# Convert (lossy!)
@docs from_list
@docs to_list
|-}


{-| A list with at least one element.-}
type alias Nonempty a =
    ( a, List a )

{-|-}
singleton : a -> Nonempty a
singleton a = ( a, [] )

{-|-}
member : a -> Nonempty a -> Bool
member x = to_list >> List.member x
              
{-| Map a function over all members of nonempty.-}
map : ( a -> b ) -> Nonempty a -> Nonempty b
map fu ( h, tail ) =
    ( fu h, List.map fu tail )

{-|-}
fold : ( a -> b -> b ) -> b -> Nonempty a -> b
fold fu previous =
    to_list >> List.foldl fu previous
        
{-| Mind: the information that it is nonempty is lost.-}
to_list : Nonempty a -> List a
to_list ( h, tail ) = h::tail

{-| Fallable conversion from List.-}
from_list : List a -> Maybe ( Nonempty a )
from_list list =
    case list of
        l::ist -> Just ( l, ist )
        _ -> Nothing
             
{-|-}
head : Nonempty a -> a
head ( h, t ) = h

{-|-}
just_singleton : Nonempty a -> Maybe a
just_singleton ( h, tail ) =
    case tail of
        [] -> Just h
        _ -> Nothing

                         
{-|-}
is_singleton : Nonempty a -> Bool
is_singleton ( h, tail ) =
    case tail of
        [] -> True
        _ -> False

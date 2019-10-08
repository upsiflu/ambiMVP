module Helpers.Nonempty exposing
  ( Nonempty
  , singleton
  , map
  , member
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

# Map
@docs map

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
map fu ( head, tail ) =
    ( fu head, List.map fu tail )

{-| Mind: the information that it is nonempty is lost.-}
to_list : Nonempty a -> List a
to_list ( head, tail ) = head::tail

{-| Fallable conversion from List.-}
from_list : List a -> Maybe ( Nonempty a )
from_list list =
    case list of
        l::ist -> Just ( l, ist )
        _ -> Nothing

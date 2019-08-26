module Compositron.Structure.Group exposing
    ( Group

    -- create
    , from_list
          
    -- read
    , to_list
    , first

    )

{-| A Zipper of multiple Branches.

# Definition
@docs Group

# Create
@docs from_list

# Read
@docs first
@docs to_list

# Map
-}

import Helpers.LZipper as LZipper exposing ( LZipper )
import Helpers exposing (..)
import Compositron.Structure.Branch as Branch exposing ( Branch )
import Maybe.Extra

{-| LZipper of Trees of nodes _a_. A focused `Branch` with its siblings.-}
type alias Group a =
    LZipper ( Branch a )

{-| Applies _may_ to close nodes and yields the first `b` that is `Just`.-}
first : ( a -> Maybe b ) -> Group a -> Maybe b
first fu grp =
    Branch.first fu grp.focus
        |> Maybe.Extra.or ( first_just ( Branch.first fu ) ( List.reverse grp.before ) )
        |> Maybe.Extra.or ( first_just ( Branch.first fu ) grp.after )
                             
       
{-|-}
to_list : Group a -> List ( Branch a )
to_list =
    LZipper.to_list

{-|-}
from_list : List ( Branch a ) -> Maybe ( Group a )
from_list =
    LZipper.from_list

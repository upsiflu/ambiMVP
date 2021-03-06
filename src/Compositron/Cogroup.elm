module Compositron.Cogroup exposing
    ( Cogroup (..)
    , is_self
    , arrows
    , first_arrow
    , serialize, deserialize
    )

{-|
# Definition
@docs Cogroup

# Read
@docs is_self
@docs arrows
@docs first_arrow

# Serial Form
@docs serialize
@docs deserialize
-}

import Helpers exposing (..)
import Helpers.Nonempty as Nonempty exposing ( Nonempty )
import Compositron.Arrow as Arrow exposing ( Arrow )

import Html exposing ( Html )
import List.Extra

{-| Opaque bundle of Arrows, where an Arrow points to a signature in `t`.
-}
type Cogroup t
    = Cogroup ( Nonempty ( Arrow t ) )

{-|-}
is_self : Cogroup t -> Bool
is_self =
    (==) ( Cogroup ( Nonempty.singleton Arrow.Self ) )
        
{-|-}
arrows : Cogroup t -> List ( Arrow t )
arrows ( Cogroup cog ) =
    cog |> Nonempty.to_list
        

{-|-}
first_arrow : Cogroup t -> Arrow t
first_arrow ( Cogroup cog ) =
    cog |> Nonempty.head
        
{-|-}
serialize :
    ( t -> String )
        -> Cogroup t
        -> String
serialize from_t ( Cogroup ( a, rrow ) ) =
    a::rrow
        |> List.map ( Arrow.serialize from_t )
        |> String.join ", "
            
{-|-}
deserialize :
    ( String -> Maybe t )
        -> String
        -> Maybe ( Cogroup t )
deserialize to_t s =
    String.split ", " s
        |> List.map ( Arrow.deserialize to_t )
        |> fold_must
        |> Maybe.andThen Nonempty.from_list
        |> Maybe.map Cogroup
                            

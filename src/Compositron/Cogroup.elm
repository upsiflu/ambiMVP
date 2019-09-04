module Compositron.Cogroup exposing
    ( Cogroup (..)
    , assumptions
    , serialize, deserialize
    , view )

{-|
# Definition
@docs Cogroup

# Read
@docs assumptions

# Serial Form
@docs serialize
@docs deserialize

# View
@docs view
-}

import Helpers exposing (..)
import Compositron.View as View exposing ( View, Action (..) )
import Html exposing ( Html )

{-|-}
type Cogroup ref
    = Referral ( Considering ref )
    | Self

type Considering ref
    = Open ( Nonempty ref )
    | Insatiable ref


{-|-}
assumptions : t -> Cogroup t -> List t
assumptions prototype cog =
    case cog of
        Self ->
            [ prototype ]
        Referral ( Open ( ref, refs ) ) ->
            ref::refs
        Referral ( Insatiable ref ) ->
            [ ref ]

{-|-}
serialize :
    ( t -> String )
        -> Cogroup t
        -> String
serialize from_t cog =
    case cog of
        Self ->
            ""
                
        Referral ( Insatiable t ) ->
            "∞ "++( from_t t )

        Referral ( Open ( t, tt ) ) ->
            ( t::tt )
                |> List.map from_t |> String.join ", " |> String.cons ' '

{-|-}
deserialize :
    ( String -> Maybe t )
        -> String
        -> Maybe ( Cogroup t )
deserialize to_t s =
    if s == "" then
        Just Self
    else
        if s |> String.startsWith " ∞ " then
             s |> String.dropLeft 3 |> to_t
                 |> Maybe.map ( Insatiable >> Referral )
         else
             s |> String.split ", " |> List.map to_t |> fold_must
                 |> Maybe.andThen to_nonempty
                 |> Maybe.map ( Open >> Referral )

    
     
                       
{-|-}
view :
    Cogroup t
        -> Map ( View node t )
view cog =
    case cog of
        Self ->
            View.action ( Class "self-assumption" )
                >> View.face "s"
                
        Referral ( Insatiable ref ) ->
            View.action ( Class "insatiable" )
                >> View.face "!"
                
        Referral ( Open _ ) ->
            View.err "Open referral in this assumption (Cogroup)."

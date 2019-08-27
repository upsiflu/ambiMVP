module Compositron.Cogroup exposing
    ( Cogroup (..)
    , assumptions
    , serialize, deserialize
    , view_option, view )

{-|
# Definition
@docs Cogroup

# Read
@docs assumptions

# Serial Form
@docs serialize
@docs deserialize

# View
@docs view_option
@docs view
-}

import Helpers exposing (..)
import Compositron.View as View exposing ( View, Action (..) )

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


                    
{-| Paint the symbol of the cogroup you can choose to manifest.



-}
view_option :
    t
    -> ( t -> View msg l t data )
    -> Cogroup t
    -> Map (View msg l t data )
view_option prototype make_child cog =
    let
        connect references child_views =
            View.children ( (++) child_views )
                >> View.add_action ( Choose_these ( Debug.log "refChoos" references ) )
                >> View.add_class ( "Cogroup" )
    in
        case cog of
            Referral ( Open ( ref, refs ) ) ->
                ( ref::refs ) |> List.map ( make_child >> View.add_class "Referral" )
                              |> connect ( ref::refs )
                                 
            Referral ( Insatiable ref ) ->
                [ ref |> make_child >> View.add_class "Insatiable" ]
                    |> connect [ ref ]
                    
            Self ->
                [ prototype |> make_child >> View.add_class "Self" ]
                    |> connect [ prototype ]

{-|-}
view :
    Cogroup t
        -> Map ( View msg l t data )
view cog =
    case cog of
        Self ->
            View.add_class "self"
                
        Referral ( Insatiable ref ) ->
            View.add_class "insatiable"
                >> View.set_text "Insatiable."
                
        Referral ( Open _ ) ->
            View.set_text "ERROR: open referral in this assumption (Cogroup)"

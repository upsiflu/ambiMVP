module History.Transformation exposing
    ( Transformation, Copy (..)
          
    -- create
    , initial             -- given a Copy, create a 'first' 
    , successive          --
        
    -- read
    , copy                -- a Do or an Undo
    , signature

    -- map
    , undone
        
    -- compare
    , is                  -- compare signature
    , compare
    -- present
    , serialize
    )

{-|Transformation is independent from State and History in
that it can work with any implementation of those modules.
Thus, one Transformation module can cope with functions
from different State implementations. The local State
implementation has to figure out how to apply the functions.
Only Serial and Signature are ever sent over the network.
This way, actual functions attached may diverge.

We make this transformation module more specific in that we
store Ordinal, Nominal and Contextual for single-dependency graphing. 

# Definition
@docs Transformation
@docs Copy

# Create
@docs initial
@docs successive

# Map
@docs undone

# Compare
@docs is
@docs compare

# Read
@docs copy
@docs signature

# Present
@docs serialize
-}

import History.Intent as Intent exposing ( .. )
import History.Transformation.Signature as Signature exposing (..)
import Helpers exposing (..)        



{-| Transformation wraps an Intent in a Copy and signs it.-}
type Transformation s
    = Transformation Signature ( Copy s )
      
{-| Copy wraps an Intent.-}
type Copy s
    = Do ( Intent s )
    | Undo Signature 
    | Undone ( Transformation s )


-- create

{-| Given a Copy, create a 'first'.-}
initial : Transformation s
initial =
    Transformation
        ( Signature.id "flupsi" )
        ( Do { serial = "trivial"
             , function = identity
             , inverse = identity } )
         
            
{-| upon an optional previous Transformation (context),
you can create a succeeding Transformation.-}        
successive : Copy s -> Map ( Transformation s )
successive c ( Transformation sig _ ) =
    Transformation ( Signature.inc sig ) c

        
do : Intent s -> Copy s
do = Do

undo : Signature -> Copy s
undo = Undo

      

-- read

{-|-}
signature : Transformation s -> Signature
signature ( Transformation s _ ) =
    s




        
{-| a `Do` or `Undo`.-}
copy : Transformation s -> Copy s                               
copy ( Transformation _ c ) =
    c


                    
-- compare

{-|-}
is : Signature -> Transformation s -> Bool
is target =
    signature >> (==) target

{-|-}
compare : ( Signature -> Signature -> Bool ) -> Transformation s -> Transformation s -> Bool
compare fu t u =
    fu ( signature t ) ( signature u )


-- map

{-|-}
undone : Transformation s -> Map ( Transformation s )
undone undone_transformation =
    map_copy ( \_-> Undone undone_transformation )

map_copy : ( Copy s -> Copy s ) -> Transformation s -> Transformation s
map_copy f ( Transformation s c ) = Transformation s ( f c )          
                
        
-- serial form

{-| Signature of the transformation, Signature of the body.-}
serialize : Transformation t -> ( String, String )
serialize t =
    case t |> both ( signature >> Signature.serialize, copy ) of
        ( serial, Do x ) ->
            ( serial, x.serial )
        ( serial, Undo sig ) ->
            ( serial, Signature.serialize sig )
        ( serial, Undone trans ) ->
            ( serial, Signature.serialize ( signature trans ) )


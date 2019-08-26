module History.Transformation exposing
    ( Transformation, Copy (..), Signature
    -- create
    , initial             -- given a Copy, create a 'first' 
    , successive          -- .
    , do                  -- 
                          --for use in <follow>.
    -- read
    , engage              -- applies your function to your state.
                          -- Identity in the cases of Curator.
    , copy                -- a Do or an Undo
    -- map
    , invert              -- invert >> invert === id
    -- compare
    , isAbove
    , isBelow
    -- present
    , serialize
    , serialize_signature
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
@docs Signature

# Create
@docs initial
@docs successive

# Do?
@docs do

# Read
@docs engage
@docs copy

# Map
@docs invert

# Compare
@docs isBelow
@docs isAbove

# Present
@docs serialize
@docs serialize_signature
-}

import History.Intent exposing ( .. )
import Helpers exposing (..)        



{-| Transformation wraps an Intent in a Copy and signs it.-}
type Transformation s
    = Transformation Signature ( Copy s )
{-| Copy wraps an Intent.-}
type Copy s
    = Do ( Intent s )

trivial : Copy s
trivial = Do { serial = "trivial", function = identity, inverse = identity }

{-|
- ordinal ―
    Locally generated increment. Unique only with Nominal.
- nominal ― 
    Remotely generated ( unique ) string. Unique to a session.
- contextual ―
    Reference to a Nominal only if there is a unique context.
-}
type alias Signature =
    { ordinal : Int                -- locally generated increment. Unique only with Nominal.
    , nominal : Nominal            -- remotely generated ( unique ) string. Unique to a session.
    , contextual : Maybe Nominal   -- reference to a Nominal only if there is a unique context.
    }

type Nominal
    = External String
    | Id String


      
-- create


{-| Given a Copy, create a 'first'.-}
initial : Transformation s
initial =
    Transformation
        { ordinal = 0
        , nominal = Id "flupsi"
        , contextual = Nothing
        } trivial
{-| upon an optional previous Transformation (context),
you can create a succeeding Transformation.-}        
successive : Transformation s -> Copy s -> Transformation s
successive previous =
    let context = signature previous
    in
    Transformation
        { ordinal = context.ordinal+1
        , nominal = Id "flupsi"
        , contextual = Just context.nominal
        }




-- specify copy

{-| Create Transformation Copies (without signature).-}
do : Intent s -> Copy s
do = Do

        
             
-- read


{-|Applies a function to a state.-}
engage : Transformation s -> Map s
engage t =
    case copy t of
        Do x -> x.function



-- modify

{-|switches inverse and function.

    invert >> invert === identity
-}
invert : Map ( Transformation s )
invert =
    mapCopy <|\c->
        case c of
            Do x -> do
                    { serial = x.serial
                    , function = x.inverse
                    , inverse = x.function }

                    
-- compare

{-| Compare `t` and `u`:

    isAbove t u =
        ordinal u > ordinal t
        || ( ordinal u == ordinal t )
            && ( nominal u > nominal t )
-}
isAbove : Transformation s -> Transformation s -> Bool
isAbove t u =
    ordinal u > ordinal t
    || ( ordinal u == ordinal t )
        && ( nominal u > nominal t )

{-| Compare `t` and `u`:

    isAbove t u =
        ordinal u < ordinal t
        || ( ordinal u == ordinal t )
            && ( nominal u < nominal t )
-}
isBelow : Transformation s -> Transformation s -> Bool
isBelow t u =
    ordinal u < ordinal t
    || ( ordinal u == ordinal t )
        && ( nominal u < nominal t )

match : Transformation s -> Transformation s -> Bool
match t u =
    signature u == signature t



-- serial form

{-|(Signature of the transformation, Signature of the body).-}
serialize : Transformation t -> ( String, String )
serialize t =
    case copy t of
        Do x -> ( serialize_signature t, x.serial )


                
-- helper readers


signature : Transformation s -> Signature
signature ( Transformation s _ ) = s

{-| a `Do` or `Undo`.-}
copy : Transformation s -> Copy s                                     
copy ( Transformation _ c ) = c

idString id = case id of 
    External s -> s
    Id s -> s

ordinal = signature >> .ordinal
            
nominal = signature >> .nominal >> idString

contextual = signature >> .contextual >> Maybe.withDefault ( Id "flupsi" ) >> idString

{-| Connect o, n, c with a dot. .-}
serialize_signature : Transformation s -> String
serialize_signature t =
    [ nominal t, ordinal t |> String.fromInt, contextual t ] |> String.join "."
             
          
-- helper mapper


mapCopy : ( Copy s -> Copy s ) -> Transformation s -> Transformation s
mapCopy f ( Transformation s c ) = Transformation s ( f c )          

mapSignature : ( Signature -> Signature ) -> Transformation s -> Transformation s
mapSignature f ( Transformation s c ) = Transformation ( f s ) c

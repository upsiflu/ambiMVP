module History.Transformation.Signature exposing
    ( Signature
    , id
    , is_below
    , is_above
    , inc
    , serialize )

{-| # Definition
@docs Signature

# Create
@docs id

# Compare
@docs is_below
@docs is_above

# Map
@docs inc

# Serial Form
@docs serialize
-}

import Helpers exposing (..)

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


-- create

{-|-}
id : String -> Signature
id name =
    { ordinal = 0
    , nominal = Id name
    , contextual = Nothing
    }

type Nominal
    = External String
    | Id String

idString ids = case ids of 
    External s -> s
    Id s -> s


ordinal = .ordinal
            
nominal = .nominal >> idString

contextual = .contextual >> Maybe.withDefault ( Id "flupsi" ) >> idString

-- map

{-|-}
inc : Map Signature
inc context =
    { ordinal = context.ordinal+1
    , nominal = Id "flupsi"
    , contextual = Just context.nominal
    }


-- compare

{-| Compare `t` and `u`:

    isAbove t u =
        ordinal u > ordinal t
        || ( ordinal u == ordinal t )
            && ( nominal u > nominal t )
-}
is_above : Signature -> Signature -> Bool
is_above t u =
    ordinal u > ordinal t
    || ( ordinal u == ordinal t )
        && ( nominal u > nominal t )

{-| Compare `t` and `u`:

    isAbove t u =
        ordinal u < ordinal t
        || ( ordinal u == ordinal t )
            && ( nominal u < nominal t )
-}
is_below : Signature -> Signature -> Bool
is_below t u =
    ordinal u < ordinal t
    || ( ordinal u == ordinal t )
        && ( nominal u < nominal t )


    
{-| Connect o, n, c with a dot. .-}
serialize : Signature -> String
serialize t =
    [ nominal t, ordinal t |> String.fromInt, contextual t ] |> String.join "."


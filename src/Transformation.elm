module Transformation exposing
    ( Transformation, engage, invert, isAbove, isBelow, create, trivial, serialize )


        
{-  TRANSFORMATION


        transform : Transformation -> State -> State
        invert : Transformation -> Transformation
            (where invert >> invert === id)
        isAbove : Transformation -> Transformation -> Bool
            (this way you can implement dependency graphs)
        isBelow : Transformation -> Transformation -> Bool


    We make this transformation module more specific in that we
    store Ordinal, Nominal and Contextual for single-dependency graphing.

    Additionally, a transformation has a serialized form. -}





type Transformation s
    = Transformation { signature : Signature
                     , serial : String
                     , function : ( s -> s ) 
                     , inverse : ( s -> s )
                     }
                   
type alias Nominal = String

type alias Signature =
    { ordinal : Int -- locally generated increment. Unique only with Nominal.
    , nominal : Nominal -- remotely generated (random, unique) string.
    , contextual : Maybe Nominal -- reference to a Nominal if there is a unique context.
    }

trivial = Debug.log "trivial transformation" <|
    Transformation { signature = { ordinal = 0, nominal = "", contextual = Nothing }
                   , serial = "trivial"
                   , function = identity
                   , inverse = identity
                   }


create : String -> ( s -> s ) -> ( s -> s ) -> Transformation s -> Transformation s
create s f i after = Debug.log "creating a singleton Transformation"
    Transformation { signature = succidencial after, serial = s, function = f, inverse = i }
                   
    
-- signature of preceding transformation
precedencial : Transformation s -> Signature
precedencial ( Transformation t ) =
    case t.signature.ordinal of
        0 -> t.signature --first entry in session <ordinal>
        o -> { ordinal = o-1, nominal = t.signature.contextual |> Maybe.withDefault "", contextual = Nothing }
succidencial : Transformation s -> Signature
succidencial ( Transformation t ) = { ordinal = t.signature.ordinal+1, nominal = t.signature.nominal, contextual = Just t.signature.nominal }

-- applying a transformation's function 
engage : Transformation s -> s -> s
engage ( Transformation t ) = t.function

-- turning a transformation around
invert : Transformation s -> Transformation s
invert ( Transformation t ) = Transformation { t | inverse = t.function, function = t.inverse, serial = String.reverse t.serial }


-- order
isAbove : Transformation s -> Transformation s -> Bool
isAbove ( Transformation t ) ( Transformation u ) =
    t.signature.ordinal > u.signature.ordinal
    || ( t.signature.ordinal == u.signature.ordinal ) && ( t.signature.ordinal > u.signature.ordinal )


isBelow : Transformation s -> Transformation s -> Bool
isBelow ( Transformation t ) ( Transformation u ) =
    t.signature.ordinal < u.signature.ordinal
    || ( t.signature.ordinal == u.signature.ordinal ) && ( t.signature.ordinal < u.signature.ordinal )






-- SERIAL FORM

serialize : Transformation t -> String
serialize ( Transformation t ) = t.serial

deserialize : String -> Transformation t
deserialize = always trivial -- TODO! It's not trivial.

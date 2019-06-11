module Transformation exposing
    ( Transformation
    , initial    -- curate an identity transformation as the basis for a history.
    , follow     -- upon any previous transformation as context.
    , engage     -- your function.
    , invert     -- invert >> invert === id
    , isAbove
    , isBelow
    , serialize )

        
{-  TRANSFORMATION

    Transformation is independent from State and History in
    that it can work with any implementation of those modules.
    Thus, one Transformation module can cope with functions
    from different State implementations. The local State
    implementation has to figure out how to apply the functions.
    Only Serial and Signature are ever sent over the network.
    This way, actual functions attached may diverge.

    We make this transformation module more specific in that we
    store Ordinal, Nominal and Contextual for single-dependency graphing. -}




type Transformation s
    = Transformation { signature : Signature
                     , serial : Serial
                     , function : ( s -> s ) 
                     , inverse : ( s -> s )
                     }

type Nominal = Nominal String -- unique to each session.
type alias Serial = String
type alias Initial s = Transformation s
    
type alias Signature =
    { ordinal : Int -- locally generated increment. Unique only with Nominal.
    , nominal : Nominal -- remotely generated ( unique ) string. Unique to a session.
    , contextual : Maybe Nominal -- reference to a Nominal only if there is a unique context.
    }


    
-- create

initial : String -> s -> Initial s
initial name state =
    Transformation { signature = { ordinal = 0, nominal = Nominal name, contextual = Nothing }
                   , serial = "trivial"  -- canonical serialization of the function
                   , function = identity -- cache
                   , inverse = identity  -- cache
                   }

follow : Initial s -> Transformation s -> ( Serial -> ( s -> s ) -> ( s-> s ) -> Transformation s )
follow ( Transformation ini ) ( Transformation con ) =
    \ser f i ->
        let sig = { ordinal = con.signature.ordinal+1
                  , nominal = ini.signature.nominal
                  , contextual = Just con.signature.nominal }
        in Transformation
            { signature = sig, serial = ser, function = f, inverse = i }




precedencial : Transformation s -> Signature
precedencial ( Transformation t ) =
    case t.signature.ordinal of
        0 -> t.signature --first entry in session <ordinal>
        o -> { ordinal = o-1, nominal = t.signature.contextual |> Maybe.withDefault ( Nominal "" ), contextual = Nothing }


             
-- read

engage : Transformation s -> s -> s
engage ( Transformation t ) = t.function


                              
-- modify

invert : Transformation s -> Transformation s
invert ( Transformation t ) = Transformation { t | inverse = t.function, function = t.inverse, serial = String.reverse t.serial }


                              
-- compare

gt ( Nominal n ) ( Nominal o ) = n>o
st ( Nominal n ) ( Nominal o ) = n<o
                                 
isAbove : Transformation s -> Transformation s -> Bool
isAbove ( Transformation t ) ( Transformation u ) =
    u.signature.ordinal > t.signature.ordinal
    || ( t.signature.ordinal == u.signature.ordinal ) && ( gt u.signature.nominal t.signature.nominal )

isBelow : Transformation s -> Transformation s -> Bool
isBelow ( Transformation t ) ( Transformation u ) =
    u.signature.ordinal < t.signature.ordinal
    || ( t.signature.ordinal == u.signature.ordinal ) && ( st u.signature.nominal t.signature.nominal )





-- SERIAL FORM

serialize : Transformation t -> String
serialize ( Transformation t ) = t.serial


module History.Transformation exposing
    ( Transformation, Copy (..)
    -- create
    , initial             -- given a Copy, create a 'first' 
    , successive          -- upon an optional previous Transformation (context),
                          -- you can create a succeeding Transformation.
    , do                  -- create Transformation Copies (without signature)
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

import History.Intent exposing ( .. )
        
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
    = Transformation Signature ( Copy s )

type Copy s
    = Do ( Intent s )

trivial : Copy s
trivial = Do { serial = "trivial", function = identity, inverse = identity }
      
type alias Signature =
    { ordinal : Int                -- locally generated increment. Unique only with Nominal.
    , nominal : Nominal            -- remotely generated ( unique ) string. Unique to a session.
    , contextual : Maybe Nominal   -- reference to a Nominal only if there is a unique context.
    }

type Nominal
    = External String
    | Id String


      
-- create

initial : Transformation s
initial =
    Transformation
        { ordinal = 0
        , nominal = Id "flupsi"
        , contextual = Nothing
        } trivial
        
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


do = Do

        
             
-- read


engage : Transformation s -> s -> s
engage t =
    case copy t of
        Do x -> x.function



-- modify


invert : Transformation s -> Transformation s
invert =
    mapCopy <|\c->
        case c of
            Do x -> do
                    { serial = x.serial
                    , function = x.inverse
                    , inverse = x.function }

                    
-- compare


isAbove : Transformation s -> Transformation s -> Bool
isAbove t u =
    ordinal u > ordinal t
    || ( ordinal u == ordinal t )
        && ( nominal u > nominal t )

isBelow : Transformation s -> Transformation s -> Bool
isBelow t u =
    ordinal u < ordinal t
    || ( ordinal u == ordinal t )
        && ( nominal u < nominal t )

match : Transformation s -> Transformation s -> Bool
match t u =
    signature u == signature t



-- serial form


serialize : Transformation t -> ( String, String )
serialize t =
    case copy t of
        Do x -> ( serialize_signature t, x.serial )


                
-- helper readers


signature : Transformation s -> Signature
signature ( Transformation s _ ) = s

copy : Transformation s -> Copy s                                     
copy ( Transformation _ c ) = c

idString id = case id of 
    External s -> s
    Id s -> s

nominal = signature >> .nominal >> idString

ordinal = signature >> .ordinal

contextual = signature >> .contextual >> Maybe.withDefault ( Id "flupsi" ) >> idString

serialize_signature t =
    [ nominal t, ordinal t |> String.fromInt, contextual t ] |> String.join "."
             
          
-- helper mapper


mapCopy : ( Copy s -> Copy s ) -> Transformation s -> Transformation s
mapCopy f ( Transformation s c ) = Transformation s ( f c )          

mapSignature : ( Signature -> Signature ) -> Transformation s -> Transformation s
mapSignature f ( Transformation s c ) = Transformation ( f s ) c

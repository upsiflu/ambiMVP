module Transformation exposing
    ( Transformation, Copy
    -- create
    , initial             -- given a Copy, create a 'first' 
    , follow              -- upon an optional previous Transformation (context), you can create Do and Undo Transformations.
    , do, undo            -- create Transformation Copies (without signature) for use in <follow>.
    -- read
    , nominal             -- from the signature
    , engage              -- applies your function to your state. Identity in the cases of Curator and Undo.
    -- map
    , invert              -- invert >> invert === id
    -- compare
    , isAbove
    , isBelow
    -- present
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
    = Transformation Signature ( Copy s )

type Copy s
    = Do { serial : String
         , function : ( s -> s ) 
         , inverse : ( s -> s )
         }
    | Undo Signature

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

initial : Copy s -> Transformation s
initial =
    Transformation
        { ordinal = 0
        , nominal = Id "flupsi"
        , contextual = Nothing
        }
        
follow : Maybe ( Transformation s ) -> ( Copy s -> Transformation s )
follow =
    Maybe.withDefault ( initial trivial ) >> signature >> \context ->
        Transformation
            { ordinal = context.ordinal+1
            , nominal = Id "flupsi"
            , contextual = Just context.nominal
            }



-- specify copy

do = Do

undo : Transformation s -> Copy s
undo =
    signature >> Undo

        
             
-- read

engage : Transformation s -> s -> s
engage t =
    case copy t of
        Do x -> x.function
        _    -> identity
        




-- modify

invert : Transformation s -> Transformation s
invert =
    mapCopy <|\c->
        case c of
            Do x -> do { serial = ( String.reverse x.serial ), function = x.inverse, inverse = x.function }
            _    -> c

                              
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

serialize : Transformation t -> String
serialize t =
    case copy t of
        Do x -> x.serial
        Undo u -> "undo " ++ ( idString u.nominal ) ++ "#" ++ ( String.fromInt u.ordinal )




-- helper readers

signature ( Transformation sig _ ) = sig

copy : Transformation s -> Copy s                                     
copy ( Transformation _ cop ) = cop

idString id = case id of 
    External s -> s
    Id s -> s
                                
nominal = signature >> .nominal >> idString
                                            
ordinal = signature >> .ordinal

-- helper mapper
    
mapCopy : ( Copy s -> Copy s ) -> Transformation s -> Transformation s
mapCopy f ( Transformation s c ) = Transformation s ( f c )          

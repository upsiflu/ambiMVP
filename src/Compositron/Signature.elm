module Compositron.Signature exposing
    ( Signature
    , Creator

    -- map
    , accept
        
    -- create
    , root
    , ephemeral
    , prime
    , inc

    -- view
    , view
    , serialize
    , deserialize
    )

import Compositron.View as View exposing ( View, Action (..) )

import Helpers exposing (..)


{-| Signature

If you call root only once, 
and call create only with unique strings,
then each signature will be unique.

|-} 


type alias Creator = String

type Signature domain =
    Signature { creator : Creator, scalar : Int }


        
-- create

        
root : Signature domain
root = prime "upsiflu" |> inc


inc : Map ( Signature domain )
inc = map_scalar ( (+) 1 )


prime : String -> Signature prime
prime cre = Signature { creator = cre, scalar = -1 }

ephemeral : Int -> Signature domain
ephemeral n = Signature { creator = "ephemeral", scalar = n }

             
-- map

accept : Signature p -> Signature l
accept ( Signature primer ) =
        Signature { creator = primer.creator, scalar = primer.scalar }


             
map_scalar fu =
    \( Signature sig ) -> Signature { sig | scalar = fu sig.scalar }

map_creator fu =
    \( Signature sig ) -> Signature { sig | creator = fu sig.creator }

scale = always >> map_scalar
             
dec = map_scalar ( (-) 1 )



-- serialize


serialize : Signature domain -> String
serialize ( Signature sig ) =
    sig.creator ++ "/" ++ ( String.fromInt sig.scalar )

deserialize : String -> Maybe ( Signature domain )
deserialize str =
    case String.split "/" str |> List.reverse of

        x::xs ->
           case ( String.toInt x
                , String.join "/" xs )
           of
               ( Just sca, cre ) ->
                   Just ( Signature { creator = cre, scalar = sca } )
               _ ->
                   trace
                   ("Parts of signature '"++str++"' could not be parsed.")
                       Nothing

        _ ->
            trace "An empty string failed to parse as signature."
                Nothing
    


-- view


view : Signature domain -> Map ( View msg ( Signature domain ) cosig data )
view signature =
    View.add_id <| serialize signature

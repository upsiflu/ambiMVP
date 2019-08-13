module Compositron.Signature exposing
    ( Signature
    , Creator

    -- create
    , root
    , ephemeral
    , create
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

        
root : domain -> Signature domain
root d = create "upsiflu" d |> inc


inc : Map ( Signature domain )
inc = map_scalar ( (+) 1 )


create : String -> domain -> Signature domain
create cre d = Signature { creator = cre, scalar = -1 }

ephemeral : Int -> domain -> Signature domain
ephemeral n d = Signature { creator = "ephemeral", scalar = n }

             

             
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

deserialize : String -> domain -> Maybe ( Signature domain )
deserialize str d =
    case String.split "/" str |> List.reverse of

        x::xs ->
           case ( String.toInt x
                , String.join "/" xs )
           of
               ( Just sca, cre ) ->
                   Just ( Signature { creator = cre, scalar = sca } )
               _ ->
                   trace ("Parts of the signature '"++str++"' could not be parsed.")
                       Nothing

        _ ->
            trace "An empty string failed to parse as signature."
                Nothing
    


-- view


view : Signature domain -> Map ( View msg item ( Signature domain ) data )
view signature =
    View.add_id <| serialize signature

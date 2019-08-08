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

type alias Signature domain =
    { creator : Creator, scalar : Int }


        
-- create

        
root : domain -> Signature domain
root = create "upsiflu" d |> inc


inc : Map Signature
inc = map_scalar ( (+) 1 )


create : String -> domain -> Signature domain
create cre d = { creator = cre, scalar = -1 }

ephemeral : Int -> domain -> Signature domain
ephemeral n d = { creator = "ephemeral", scalar = n }

             

             
map_scalar fu =
    \sig -> { sig | scalar = fu sig.scalar }

map_creator fu =
    \sig -> { sig | creator = fu sig.creator }

scale = always >> map_scalar
             
dec = map_scalar ( (-) 1 )



-- serialize


serialize : Signature domain -> String
serialize signature =
    signature.creator ++ "/" ++ ( String.fromInt signature.scalar )

deserialize : String -> domain -> Maybe ( Signature domain )
deserialize str d =
    case String.split "/" str |> List.reverse of

        x::xs ->
           case ( String.toInt x
                , String.join "/" xs )
           of
               ( Just sca, cre ) ->
                   Just { creator = cre, scalar = sca }
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

module Compositron.Signature exposing
    ( Signature
    , Creator
    , root
    , create
    , inc
    )

import Compositron.View as View exposing ( View, Action (..) )

import Helpers exposing (..)




{-| Signature

If you call root only once, 
and call create only with unique strings,
then each signature will be unique.

|-} 


type alias Creator = String

type alias Signature =
    { creator : Creator, scalar : Int }


        
-- create

        
root : Signature
root = { creator = "upsiflu", scalar = 0 }


inc : Map Signature
inc = map_scalar ( (+) 1 )


create : String -> Signature
create cre = { creator = cre, scalar = 0 }



             
map_scalar fu =
    \sig -> { sig | scalar = fu sig.scalar }

map_creator fu =
    \sig -> { sig | creator = fu sig.creator }

scale = always >> map_scalar

create cre = { creator = cre, scalar = 0 }
             
dec = map_scalar ( (-) 1 )



-- serialize


serialize : Signature -> String
serialize signature =
    signature.creator ++ "/" ++ ( String.fromInt signature.scalar )



-- view


view : Signature -> Map ( View msg item Signature data )
view signature =
    View.add_id <| serialize signature

module Compositron.Signature exposing (..)

import Compositron.View as View exposing ( View, Action (..) )

import Helpers exposing (..)


-- Signature


type alias Signature =
    { creator : String, scalar : Int }


root = { creator = "upsiflu", scalar = 0 }
        
map_scalar fu =
    \sig -> { sig | scalar = fu sig.scalar }

map_creator fu =
    \sig -> { sig | creator = fu sig.creator }

scale = always >> map_scalar

create cre = { creator = cre, scalar = 0 }

irrelevant n = { creator = "", scalar = n }


             
dec = map_scalar ( (-) 1 )
inc = map_scalar ( (+) 1 )



-- serialize


serialize : Signature -> String
serialize signature =
    signature.creator ++ "/" ++ ( String.fromInt signature.scalar )



-- view


view : Signature -> Map ( View msg item Signature data )
view signature =
    View.add_id <| serialize signature

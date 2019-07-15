module Compositron.Node exposing (..)


import Helpers exposing (..)

import Compositron.Item as Item exposing ( Item )
import Compositron.Data as Data exposing ( Data )
import Compositron.Signature as Signature exposing ( Signature )
import Compositron.Manifestation as Manifestation exposing ( Manifestation )

import Compositron.View as View exposing ( View, Action (..) )


-- Node


type alias Node =
    { signature : Signature
    , item : Item
    , manifestation : Manifestation
    }

trivial : Node
trivial =
     { signature = Signature.root
     , item = Item.Info "ROOT"
     , manifestation = Manifestation.passive }
    

-- map


set_index : Int -> Map Node
set_index n =
    map_signature <| Signature.scale n
        
map_signature : Map Signature -> Map Node
map_signature fu =
    \this -> { this | signature = fu this.signature }

map_item : Map Item -> Map Node
map_item fu =
    \this-> { this | item = fu this.item }   

map_manifestation : Map Manifestation -> Map Node
map_manifestation fu =
    \this-> { this | manifestation = fu this.manifestation }   



view : Node -> Map ( View msg Item Signature Data )
view node =
    Signature.view node.signature
        >> Item.view node.item
        >> Manifestation.view node.manifestation


serialize : Node -> String
serialize node =
    ( Manifestation.serialize node.manifestation )
     ++ " " ++ ( Signature.serialize node.signature )
        ++ ": " ++ ( Item.serialize node.item ) 

module Compositron.Node exposing (..)


import Helpers exposing (..)

import Compositron.Item as Item exposing ( Item )
import Compositron.Data as Data exposing ( Data )
import Compositron.Signature as Signature exposing ( Signature )
import Compositron.Manifestation as Manifestation exposing ( Manifestation )

import Compositron.View as View exposing ( View, Action (..) )



{-| Node 

is the content type of its instance,
which is probably a Compositron.

 |-}

type alias Node instance =
    { signature : Signature
    , item : Item instance
    , manifestation : Manifestation
    }



-- create

    
trivial : Node instance
trivial =
     Signature.root |> create ( Item.Info "ROOT" )
             
    
create : Item instance -> Signature.Creator -> Node instance
create itm cre =
     { signature = Signature.create cre
     , item = itm
     , manifestation = Manifestation.passive }


inc : Item instance -> Map ( Node instance )
inc itm =
    map_signature Signature.inc


    

-- read



    
-- map


set_index : Int -> Map ( Node instance )
set_index n =
    map_signature <| Signature.scale n
        
map_signature : Map Signature -> Map Node
map_signature fu =
    \this -> { this | signature = fu this.signature }

map_item : Map ( Item instance ) -> Map ( Node instance )
map_item fu =
    \this-> { this | item = fu this.item }   

map_manifestation : Map Manifestation -> Map ( Node instance )
map_manifestation fu =
    \this-> { this | manifestation = fu this.manifestation }   



view : Node instance -> Map ( View msg ( Item instance ) Signature Data )
view node =
    Signature.view node.signature
        >> Item.view node.item
        >> Manifestation.view node.manifestation


serialize : Node instance -> String
serialize node =
    ( Manifestation.serialize node.manifestation )
     ++ " " ++ ( Signature.serialize node.signature )
        ++ ": " ++ ( Item.serialize node.item ) 

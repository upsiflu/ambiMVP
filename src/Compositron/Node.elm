module Compositron.Node exposing (..)


import Helpers exposing (..)

import Compositron.Item as Item exposing ( Item )
import Compositron.Data as Data exposing ( Data )
import Compositron.Signature as Signature exposing ( Signature )
import Compositron.Manifestation as Manifestation exposing ( Manifestation )

import Compositron.View as View exposing ( View, Action (..) )




type alias Node sig cosig =
    { signature : sig
    , prototype : cosig
    , item : Item sig cosig
    , manifestation : Manifestation
    }



-- create

    
trivial : Node sig cosig
trivial =
    { signature = Signature.root
    , prototype = Signature.root
    , item = Item.paragraph
    , manifestation = Manifestation.active
    }
             


inc : Map ( Node sig cosig )
inc = map_signature Signature.inc


accept : Node t t -> Node p t -> Node l t
accept tmp prm =
    { tmp | signature = prm.signature }



-- read


items_equal : Node s0 c0 -> Node s1 c1 -> Bool
items_equal node0 node1 =
    node0.item == node1.item
        
    
-- map


map_domain : d0 -> d1 -> Node d0 c -> Node d1 c
map_domain =
    \_ _ -> identity

map_signature : Map sig -> Map ( Node sig cosig )
map_signature fu =
    \this -> { this | signature = fu this.signature }

map_prototype : Map cosig -> Map ( Node sig cosig )
map_prototyp fu =
    \this -> { this | prototype = fu this.prototype }

             
map_item : Map ( Item sig cosig ) -> Map ( Node sig cosig )
map_item fu =
    \this-> { this | item = fu this.item }   

map_manifestation : Map Manifestation -> Map ( Node sig cosig )
map_manifestation fu =
    \this-> { this | manifestation = fu this.manifestation }   


            
-- set


set_item = always >> map_item
set_manifestation = always >> map_manifestation

activate : Map ( Node sig cosig )
activate = set_manifestation Manifestation.active

passivate : Map ( Node sig cosig )
passivate = set_manifestation Manifestation.passive


                    
--view
                    

view : Node sig cosig -> Map ( View msg ( Item sig cosig ) sig Data )
view node =
    Signature.view node.signature
        >> Item.view node.item
        >> Manifestation.view node.manifestation


serialize : Node sig cosig -> String
serialize node =
    ( Manifestation.serialize node.manifestation )
        ++ ( Signature.serialize node.signature ) ++ ": "
        ++ ( Item.serialize node.item ) 

deserialize : String -> Maybe ( Node cosig )
deserialize str =
    case String.split (": ") str of

        x::xs ->
            case ( Signature.deserialize ( String.dropLeft 1 x )
                 , Item.deserialize ( String.join ": " xs )
                 , Manifestation.deserialize ( String.left 1 x )
                 )
            of
                ( Just s, Just i, Just m ) ->
                    Just
                        { signature = s 
                        , item = i
                        , manifestation = m 
                        }
                        
                _ ->
                    trace "Parts of the node could not be parsed."
                        Nothing
            
        [] ->
             trace "Empty String parsed as Node."
                 Nothing

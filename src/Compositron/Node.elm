module Compositron.Node exposing (..)


import Helpers exposing (..)

import Compositron.Item as Item exposing ( Item )
import Compositron.Data as Data exposing ( Data )
import Compositron.Signature as Signature exposing ( Signature )
import Compositron.Manifestation as Manifestation exposing ( Manifestation )

import Compositron.View as View exposing ( View, Action (..) )




type alias Node domain =
    { signature : Signature domain
    , item : Item ( Signature domain )
    , manifestation : Manifestation
    }



-- create

    
trivial : Node domain
trivial =
    { signature = Signature.root
    , item = Item.paragraph
    , manifestation = Manifestation.active
    }
             

primer = create Item.primer
         
create : Item ( Signature domain ) -> Signature.Creator -> Node domain
create itm cre =
     { signature = Signature.create cre
     , item = itm
     , manifestation = Manifestation.passive
     }


inc : Item ( Signature domain ) -> Map ( Node domain )
inc itm =
    map_signature Signature.inc




    
-- map

{-
set_index : Int -> Map ( Node instance )
set_index n =
    map_signature <| Signature.scale n
  -}      
map_signature : Map ( Signature domain ) -> Map ( Node domain )
map_signature fu =
    \this -> { this | signature = fu this.signature }

map_item : Map ( Item ( Signature domain ) ) -> Map ( Node domain )
map_item fu =
    \this-> { this | item = fu this.item }   

map_manifestation : Map Manifestation -> Map ( Node domain )
map_manifestation fu =
    \this-> { this | manifestation = fu this.manifestation }   


-- set

set_item = always >> map_item
set_manifestation = always >> map_manifestation

activate : Map ( Node domain )
activate = set_manifestation Manifestation.active

passivate : Map ( Node domain )
passivate = set_manifestation Manifestation.passive


                    
--view
                    

view : Node domain -> Map ( View msg ( Item ( Signature domain ) ) ( Signature domain ) Data )
view node =
    Signature.view node.signature
        >> Item.view node.item
        >> Manifestation.view node.manifestation


serialize : Node domain -> String
serialize node =
    ( Manifestation.serialize node.manifestation )
        ++ ( Signature.serialize node.signature ) ++ ": "
        ++ ( Item.serialize node.item ) 

deserialize : String -> Maybe ( Node domain )
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

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

    
trivial : Node ( Signature codomain ) ( Signature codomain )
trivial =
    { signature = Signature.root
    , prototype = Signature.root
    , item = Item.Info "trivial"
    , manifestation = Manifestation.active
    }
             
primer : Signature.Creator -> Node ( Signature prime ) ( Signature prime )
primer creator =
    { signature = Signature.prime creator
    , prototype = Signature.prime creator
    , item = Item.Err "primer"
    , manifestation = Manifestation.passive
    }

    
inc : Map ( Node ( Signature prime ) ( Signature prime ) )
inc = map_signature Signature.inc


accept : Node ( Signature t ) ( Signature t ) ->
         Node ( Signature p ) ( Signature p ) ->
         Node ( Signature l ) ( Signature t ) 
accept tmp prm =
    { signature = Signature.accept prm.signature
    , prototype = tmp.prototype
    , item = Item.accept tmp.item
    , manifestation = Manifestation.passive
    }


-- read


equal_items : Node ( Signature d ) ( Signature c ) ->
              Node ( Signature c ) ( Signature c ) ->
                  Bool
equal_items node conode =
    Item.equal node.item conode.item
        
self_reference : Node ( Signature d ) ( Signature c ) -> Maybe ( Signature c )
self_reference nod =
    if Item.is_self_assumption nod.item then Just nod.prototype else Nothing

        
-- map


map_domain :
    Node ( Signature c ) ( Signature c ) ->
        Node ( Signature d1 ) ( Signature c )
map_domain =
    \this -> { signature = Signature.accept this.signature
             , prototype = this.prototype
             , item = Item.accept this.item
             , manifestation = this.manifestation
             }

map_signature : Map sig -> Map ( Node sig cosig )
map_signature fu =
    \this -> { this | signature = fu this.signature }

map_prototype : Map cosig -> Map ( Node sig cosig )
map_prototype fu =
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
                    

view :
    Node ( Signature domain ) cosig ->
        Map ( View msg ( Signature domain ) cosig Data )
view node =
    Signature.view node.signature
        >> Item.view node.item
        >> Manifestation.view node.manifestation


serialize : Node ( Signature domain ) ( Signature codomain ) -> String
serialize node =
    ( Manifestation.serialize node.manifestation )
        ++ ( Signature.serialize node.signature )
        ++ " ("
        ++ ( Signature.serialize node.prototype )
        ++ "): "
        ++ ( Item.serialize Signature.serialize Signature.serialize node.item )

deserialize :
    String ->
        Maybe ( Node ( Signature codomain ) ( Signature codomain ) )
deserialize str =
    case String.split ("): ") str of

        x::xs ->
            let
                incipit :
                    ( Maybe ( Signature domain )
                    , Maybe ( Signature codomain )
                    )
                incipit =
                    String.split " (" x
                        |> both ( List.head
                                , List.tail
                                    >> Maybe.map ( String.join " (" )
                                )
                        |> each ( Maybe.andThen ( Signature.deserialize ) )
                        |> Tuple.mapSecond ( Maybe.map Signature.accept )
                    
            in case
                ( incipit
                , String.join "): " xs
                    |> Item.deserialize
                       Signature.deserialize Signature.deserialize
                , String.left 1 x |> Manifestation.deserialize
                )
            of
                ( ( Just s, Just p ), i, Just m ) ->
                    Just
                        { signature = s
                        , prototype = p
                        , item = i
                        , manifestation = m 
                        }
                        
                _ ->
                    trace ( "Parts of the node ("++str++") could not be parsed." )
                        Nothing
            
        [] ->
             trace "Empty String parsed as Node."
                 Nothing

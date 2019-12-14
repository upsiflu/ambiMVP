module Compositron.Node exposing
    ( Node
          
    -- create
    , primer
    , error
    , adopt_domain
    , accept
        
    -- read
    , equal_items
        
    -- set
    , set_item
    , set_manifestation
    , inc

    -- map
    , map_item
        
    -- serial form
    , serialize, deserialize
    , view
    )

{-|
# Definition
@docs Node

# Create
@docs primer
@docs error

Create by shifting domains:
@docs accept
@docs adopt_domain

# Read
@docs equal_items

# Set
@docs set_item
@docs inc
@docs set_manifestation

# Map
@docs map_item

# Serial Form
@docs serialize
@docs deserialize

# View
@docs view

-}
import Maybe.Extra

import Helpers exposing (..)
import Helpers.Nonempty as Nonempty

import Compositron.Item as Item exposing ( Item )
import Compositron.Data as Data exposing ( Data )
import Compositron.Signature as Signature exposing ( Signature )
import Compositron.Manifestation as Manifestation exposing ( Manifestation (..) )

import Compositron.View as View exposing ( View, Action (..) )



{-|-}
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
    , item = Item.Error "trivial"
    , manifestation = Notargeted
    }

{-|-}
error : Node ( Signature codomain ) ( Signature codomain )
error =
    trivial |> map_item ( \_-> Item.Error "Error: Check the console for details." )

{-|-}
primer : Signature.Creator -> Node ( Signature prime ) ( Signature prime )
primer creator =
    { signature = Signature.prime creator
    , prototype = Signature.prime creator
    , item = Item.Error "primer"
    , manifestation = Notargeted
    }

{-|-}
inc : Map ( Node ( Signature prime ) ( Signature prime ) )
inc = map_signature Signature.inc

{-|-}
accept : Node ( Signature p ) ( Signature p ) ->
         Node ( Signature t ) ( Signature t ) ->
         Node ( Signature l ) ( Signature t ) 
accept prm tmp =
    { signature = Signature.accept prm.signature
    , prototype = tmp.prototype
    , item = Item.accept tmp.item
    , manifestation = prm.manifestation
    }


-- read

{-|-}
equal_items :
    Node ( Signature d ) ( Signature c ) ->
    Node ( Signature c ) ( Signature c ) ->
    Bool
equal_items node conode =
    Item.equal node.item conode.item

{- If an Assumption is unambiguous, it has `Just` one Cogroup.

first_reference : Node ( Signature d ) ( Signature c ) -> Maybe ( Signature c )
first_reference nod =
    nod.item
        |> Item.options
        |> Nonempty.just_singleton -- weed out option ambiguations
        |> Maybe.map Cogroup.first_arrow
        |> Maybe.map
           ( \arrow -> if arrow == Self then nod.prototype else 
-}
        
-- map

{-|-}
adopt_domain :
    Node ( Signature c ) ( Signature c ) ->
    Node ( Signature d1 ) ( Signature c )
adopt_domain =
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

{-|-}
map_item : Map ( Item sig cosig ) -> Map ( Node sig cosig )
map_item fu =
    \this-> { this | item = fu this.item }   

map_manifestation : Map Manifestation -> Map ( Node sig cosig )
map_manifestation fu =
    \this-> { this | manifestation = fu this.manifestation }   

            
-- set

{-|-}
set_item : Item sig cosig -> Map ( Node sig cosig )
set_item itm =
    \this-> { this | item = itm }

{-|-}
set_manifestation : Manifestation -> Map ( Node sig cosig )
set_manifestation = always >> map_manifestation

                    
-- serial form


{-|-}
serialize : Node ( Signature domain ) ( Signature codomain ) -> String
serialize node =
        ( Item.serialize Signature.serialize Signature.serialize node.item )
        ++ " :"
        ++ ( Signature.serialize node.signature )
        ++ "\t("
        ++ ( Signature.serialize node.prototype )
        ++ ")"
        ++ Manifestation.serialize node.manifestation

{-|-}            
deserialize :
    String ->
        Maybe ( Node ( Signature codomain ) ( Signature codomain ) )
deserialize str =
    let ( st, suffix ) =
            case String.right 1 str of
                ")" -> ( str, "" )
                suf -> ( String.dropRight 1 str, suf )
    in
      case String.split (" :") st of

        x::xs ->
            let
                signatures :
                    ( Maybe ( Signature codomain )
                    , Maybe ( Signature codomain )
                    )
                signatures =
                    xs  |> String.join " :"
                        |> String.split "\t("
                        |> both ( List.head
                                , List.tail
                                    >> Maybe.map ( String.join "\t(" )
                                    >> Maybe.map ( String.dropRight 1 )
                                )
                        |> each ( Maybe.andThen ( Signature.deserialize ) )
                    
            in case
                ( signatures
                , Item.deserialize
                       Signature.deserialize Signature.deserialize x
                , suffix |> Manifestation.deserialize
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
                    trace ( "Parts of the node {"++str++"} could not be parsed." )
                        Nothing
            
        [] ->
             trace "Empty String parsed as Node."
                 Nothing



--view
                    
{-|-}
view :
    Node ( Signature domain ) ( Signature codomain ) ->
        View ( Node ( Signature domain ) ( Signature codomain ) ) ( Signature codomain )
view node =
    View.live
        ( Signature.serialize node.signature ) node node.manifestation
    |> Item.view
        node.prototype node.item

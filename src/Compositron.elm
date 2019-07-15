module Compositron exposing
    ( State, trivial, preview )

import Tree exposing ( Tree )
import Tree.Zipper as Zipper exposing  ( Zipper, mapLabel, root, tree, forward, append, findFromRoot, fromTree )
import Tuple exposing ( first, second )

import Browser.Dom as Dom
import Debug
import Html exposing ( Html )
import Html.Attributes exposing  ( .. )
import Html.Events exposing ( .. )
import Html.Lazy exposing ( .. )
import Json.Decode as Json
import Set exposing ( Set )
import Task

import Compositron.Item as Item exposing ( Item )
import Compositron.Node as Node exposing ( Node )
import Compositron.Manifestation as Manifestation exposing ( Manifestation )
import Compositron.Data as Data exposing ( Data )
import Compositron.Signature as Signature exposing ( Signature )
import Compositron.View as View exposing ( View, Action (..) )

import History.Intent exposing ( Intent )

import Helpers exposing ( .. )



-- Compositron


type alias State = Compositron
type alias Compositron = Zipper Node -- has focus (hole)
type alias Compositree = Tree Node   -- same, but has no focus
 

                   
-- create


trivial : Compositron
trivial =
    singleton ( Node.trivial )
        |> map_children ( always ( List.repeat 5 <| singleton Node.trivial ) )
        |> map_children ( List.indexedMap ( \n c -> set_index n c ) )
        |> map_each_child ( set_item Item.paragraph )
        |> map_each_child ( map_creator (always "child" ) )
        --|> Debug.log "before unfold"
        |> map_each_child unfold
        --|> Debug.log "after unfold"
           
singleton : Node -> Compositron
singleton nod =
    Tree.singleton nod |> Zipper.fromTree

        
branch : Map Signature -> Map Compositron
branch sigmap =
    Zipper.tree >> Zipper.fromTree >> map_signature sigmap

        
        
-- read


-- elements
node = Zipper.label
manifestation = node >> .manifestation
signature = node >> .signature
item = node >> .item
data = item >> Item.data
parent = \z -> z |> Zipper.parent |> Maybe.withDefault ( Zipper.root z )

-- emptiness : Bool
is_empty z =
    item z |> Item.is_empty ( always ( are_children_empty z ) )
are_children_empty =
    Zipper.children >> List.all ( Zipper.fromTree >> is_empty )
are_siblings_empty z =
    z |> Zipper.parent >> Maybe.map are_children_empty >> Maybe.withDefault ( is_empty z )

        

-- change node


map_node : Map Node -> Map Compositron
map_node = Zipper.mapLabel

map_signature : Map Signature -> Map Compositron
map_signature = Node.map_signature >> map_node 
map_creator c = map_signature ( Signature.map_creator c )
inc = map_signature ( Signature.inc )
dec = map_signature ( Signature.dec )
set_index i = map_signature ( Signature.scale i )
      
map_item = Node.map_item >> map_node
set_item = always >> map_item
map_manifestation = Node.map_manifestation >> map_node 

                          

-- map                    
                

-- turn the item's child items into fresh child compositrons
unfold : Map Compositron
unfold z =
    let
        item_to_compositron itm = branch Signature.inc z |> set_item itm
        chitems = ( ( item z ) ) |> Item.children
        new_children = ( chitems )
            |> List.map item_to_compositron
            |> List.indexedMap set_index
            |> List.map unfold
    in
        z |> map_children ( (++) ( new_children ) )
                   
-- map each sibling, including the targeted compositron.
map_each_sibling : Map Compositron -> Map Compositron
map_each_sibling = map_each_child >> map_parent

-- map the parent Compositron if it exists.
map_parent : Map Compositron -> Map Compositron
map_parent fu z =
    z   |> Zipper.parent
        |> Maybe.map ( fu >> Zipper.findNext ( (==) ( node z ) ) >> Maybe.withDefault z )
        |> Maybe.withDefault z

-- modify each child.
map_each_child : Map Compositron -> Map Compositron
map_each_child fu z =
    z   |> Zipper.firstChild
        |> Maybe.map ( while_just fu Zipper.nextSibling >> Zipper.parent >> Maybe.withDefault z )
        |> Maybe.withDefault z



-- change structure   


-- keep only siblings that fit a predicate
filter_siblings : ( Compositron -> Bool ) -> Map Compositron
filter_siblings =
    List.filter >> map_children
        
-- insert a sibling for each match
copy_as_sibling_where : ( Compositron -> Bool ) -> Map Compositron
copy_as_sibling_where predicate =
    let
        step sib ( list, instance ) =
            if predicate sib
            then ( ( inc instance ) :: sib :: list, inc instance )
            else ( sib :: list, instance )
        add_copies_of template =
            List.foldr step ( [], template ) >> Tuple.first
    in
        \z -> z |> map_siblings ( add_copies_of z )

-- map the list of siblings, including the targeted compositron.
map_siblings : Map ( List Compositron ) -> Map Compositron
map_siblings = map_children >> map_parent

trace mess z =
    let heading fu = Debug.log mess () |> always fu
    in ( z |> Zipper.root |> heading |> consolize 0 |> always z )
               
-- modify the list of children.
map_children : Map ( List Compositron ) -> Map Compositron
map_children fu =
    let
        over_tree =
            List.map Zipper.fromTree >> fu >> List.map Zipper.tree
    in
        trace "map_children before"
        >> Zipper.tree
        >> Tree.mapChildren over_tree
        >> Zipper.fromTree
        >> trace "map_children after"
           
-- add copies when this item is empty, remove copies when new item is empty.
prepare_emptiness : Signature -> Bool -> Map Compositron
prepare_emptiness sig will_empty =
    when ( is_empty >> (/=) will_empty )
        <| map_parent ( prepare_emptiness sig will_empty )
            >> if will_empty
               then prepare_empty sig
               else prepare_full sig

-- copy the empty node that will become full next to each "<".
prepare_full : Signature -> Map Compositron
prepare_full sig =
    copy_as_sibling_where
        ( item >> Item.is_assume_self )

-- undo the previous copying of empty items in preparation for fullness.
prepare_empty : Signature -> Map Compositron
prepare_empty sig =
    signature >> ( (/=) sig )
        |> filter_siblings

           
           
-- apply edit


target : Signature -> Map Compositron
target sig =
    findFromRoot ( \this -> this.signature == sig )
    >> Maybe.withDefault trivial

choose : Signature -> Item -> Map Compositron
choose sig future_item =
    prepare_emptiness sig ( Data.is_empty ( Item.data future_item ) ) 
        >> map_item ( always future_item )
        >> unfold
        
modify : Signature -> Data ->  Map Compositron
modify sig dat =
    prepare_emptiness sig ( Data.is_empty dat )
      >> map_item ( Item.set_data dat )

freeze : Map Compositron
freeze = map_item Item.freeze

unfreeze : Map Compositron
unfreeze = map_item Item.unfreeze

           

-- edit type

    
type Edit
    = Target Signature
    -- signature: for siblings that are to be created/removed.
    -- item: intended new.
    | Choose Signature Item
    | Modify Signature Data
    | Freeze

        
create_intent :
    Signature
        -> Node
        -> Edit
        -> Intent Compositron
create_intent new_signature this edit =
    case edit of
        -- browse the actual tree.
        Target sig ->
            { serial = [ "🞋", Signature.serialize sig ] |> String.join (" ")
            , function = target sig
            , inverse = target ( this.signature )
            }
        -- switch the item to a more concrete type or a more ambiguous one.
        Choose sig itm ->
            { serial = [ "!", Item.verbalize itm ] |> String.join (" ")
            , function = choose new_signature itm -- new sig, new itm
            , inverse = choose sig ( this.item ) -- old sig, old itm
            }
        -- switch the data in the item to be more concrete or a more empty.
        Modify sig dat ->
            { serial = [ "~", Data.enstring dat ] |> String.join (" ")
            , function = modify new_signature dat -- new sig, new dat
            , inverse = modify sig ( Item.data this.item ) -- old sig, old dat
            }
        -- make the UI render new data (workaround for contenteditable spans).
        Freeze ->
            { serial = [ "=", Data.enstring ( Item.data this.item ) ] |> String.join (" ")
            , function = freeze
            , inverse = unfreeze
            }


    
-- preview

type alias Message msg m =
    { m | from_intent : Intent Compositron -> msg
        , from_command : Cmd msg -> msg
        , noop : msg
    }

preview :
    String
        -> Message msg m
        -> Compositron
        -> List ( Html msg )
           
preview new_transformation message compositron =
    let
        targeted_compositron = map_manifestation Manifestation.target compositron |> root
        
        new_sig = Signature.create new_transformation
        
        incipit =
            Html.section []
                [ Html.h2
                      [ class "compositron", class "caption" ]
                      [ Html.text ( Signature.serialize new_sig )]
                , Html.pre
                      []
                      [ Html.text ( serialize 0 targeted_compositron ) ]
                ]
                    
        comp =
            targeted_compositron
                |> Zipper.tree
                |> view new_sig message
                |> View.present

    in [ incipit, comp ]

        
view :
    Signature
        -> Message msg m
        -> Compositree
        -> View msg Item Signature Data
        
view new_sig message compositree =
    let
        this : Node
        this = Tree.label compositree

        -- elements
                
        inner = \_->
            Tree.children compositree
            |> List.map ( view new_sig message )

        is_targeted =
            Manifestation.targeted this.manifestation
            

        -- interactivity

        to_attribute :
            Signature ->
            Action Item Data ->
            Html.Attribute msg
        to_attribute sig act = case act of
            Navigate_here -> 
                Target sig
                    |> to_message >> onClickNoBubble
            Focus_here ->
                Dom.focus ( Signature.serialize sig )
                    |> Task.attempt (\_->message.noop)
                    |> message.from_command
                    |> onClickNoBubble
            Choose_this itm ->
                Choose new_sig itm
                    |> to_message >> onClickNoBubble
            Input_url old ->
                ( Data.destring >> Modify new_sig >> to_message )
                    |> onInput
            Input_span old ->
                Json.map
                    ( Data.destring >> Modify new_sig >> to_message )
                    ( Json.at ["target", "textContent"] Json.string )
                    |> on "input"
            Blur_span ->
                Freeze
                    |> to_message >> onBlur
            Contenteditable ->
                Html.Attributes.contenteditable True
            Targeted action ->
                if is_targeted
                then to_attribute sig action
                else class ""
                                
                
        to_message =
            create_intent new_sig this
            >> message.from_intent
               
    in
        View.basic "compositron" this.signature inner
            |> Node.view this
            |> View.activate to_attribute
            


serialize : Int -> Compositron -> String
serialize depth c =
    let
        children =
            c |> Zipper.tree |> Tree.children |> List.map Zipper.fromTree
        indent = "\n" ++ ( String.repeat (1+depth) "·  " )
        serialize_children = List.map ( serialize ( depth+1 ) ) children
    in
        Node.serialize ( node c )
            ++ ( if ( List.length children > 0 )
                 then indent ++ ( serialize_children |> String.join indent )
                 else "" )

consolize : Int -> Map Compositron
consolize depth c =
    let
        children =
            c |> Zipper.tree |> Tree.children |> List.map Zipper.fromTree
        indent = String.repeat (1+depth) "·  "
        consolize_children = List.map ( consolize ( depth+1 ) ) children
    in
        consolize_children
            |> always ( Debug.log indent ( Node.serialize ( node c ) ) )
            |> always c

module Compositron exposing
    ( State
    , trivial
    , preview
    , log
    , serialize, deserialize
    , choose
    , target
    )

import Browser.Dom as Dom
import Html exposing ( Html )
import Html.Attributes exposing  ( .. )
import Html.Events exposing ( .. )
import Html.Lazy exposing ( .. )
import Json.Decode as Json
import Task



import History.Intent exposing ( Intent )

import Helpers exposing ( .. )



import Compositron.Structure.EagerZipperTree as Structure

import Compositron.Node as Node
import Compositron.Signature as Signature exposing ( Signature, Creator )
import Compositron.Item as Item
import Compositron.Manifestation as Manifestation exposing ( Manifestation )
import Compositron.Data as Data exposing ( Data )

import Compositron.View as View exposing ( View, Action (..) )





-- Compositron (=State)


type Compositron =
    Compositron { live : Structure, template : Structure }

type alias Reference = Signature
        
type alias Node = Node.Node Reference

type alias Item = Item.Item Reference

type alias State = Compositron

type alias Branch = Structure.Branch Node

type alias Structure = Structure.Structure Node
    

    
-- create


trivial : Compositron
trivial =
    Compositron { live = Structure.singleton ( Node.trivial )
                , template = Structure.singleton ( Node.trivial )
                }
 

        

-- switch


mark : Map Node -> Map Compositron
mark =
    Structure.mark >> map_live 


        
-- grow


{-- manifest: the first item to be instanciated.

 RULES.

 (1) signatures are unique, and creator matches the responsible transformation.
 (2) signatures are immutable: the target's signature remains untouched.
 (3) fields have their groups manifested as child nodes.
 (4) assumes have their template manifested in place.

--}



manifest : Map Item -> Creator -> Map Compositron
manifest itm_fu new_cre compositron =

    let
        new_node = node compositron |> Node.map_item itm_fu

        -- Given a primer and a Compositron, impose a templ
        perhaps_impose_template : Map ( Node, Structure )
        perhaps_impose_template ( pre, structure ) =

            let 
                match_template_replacement : Maybe Node -> Item -> Skippable ( Maybe ( Map Node ) )
                match_template_replacement may_live temp_item =

                  case ( Maybe.map .item may_live, temp_item ) of
                    
                    -- more template items than live nodes, accept them:
                    ( Nothing, _ ) ->
                        Node.inc temp_item |> Just |> Match
                            
                    -- compare a live item with a template item:
                    ( Just live_item, _ ) ->
                        if live_item == temp_item

                    -- live match, discard the template item here:
                        then Match Nothing

                    -- template match, accept the template item here:
                        else Node.inc temp_item |> Just |> Match
                        
 
            in
                ( pre, structure )
                    |> case Structure.node structure |> .item of
                
                           Item.Assume ( Item.Reference template_signature ) ->
                               Structure.impose_template
                                   ( template compositron
                                         |> Structure.find ( .signature >> (==) template_signature )
                                         |> Maybe.withDefault ( template compositron )
                                         |> Structure.sibling_nodes
                                         |> each ( List.map .item )
                                   )
                                   match_template_replacement
                               
                           Item.Assume Item.Self ->
                               {- todo: needs to generate prototype information,
                               then manifest here again! -}
                               identity

                           _ ->
                               identity

        
    in
        ( Node.primer new_cre, live compositron )

        --(1) change item and rebuild children
            |> Structure.replace_branch
               ( new_node, new_node.item |> Item.form )
               Node.inc

        --(2) impose templates in the siblings, manifesting each
            |> Structure.each_sibling perhaps_impose_template

        --(3) impose templates in the children, manifesting each
            |> Structure.each_child perhaps_impose_template

        -- back to Compositron, discard primer
            |> Tuple.second
            |> with compositron set_live

               
-- read


live ( Compositron c ) = c.live
template ( Compositron c ) = c.template
                         
node = live >> Structure.node
template_node = template >> Structure.node
       
signature = node >> .signature

item = node >> .item
       
manifestation =  node >> .manifestation

data = item >> Item.data




-- map


map_both : Map Structure -> Map Compositron
map_both fu = map_live fu >> map_template fu
    
map_live : Map Structure -> Map Compositron
map_live fu ( Compositron c ) = Compositron { c | live = fu c.live }

set_live : Structure -> Map Compositron
set_live = always >> map_live
                                
map_template : Map Structure -> Map Compositron
map_template fu ( Compositron c ) = Compositron { c | template = fu c.template }


                                    
                                
-- apply edit


type Edit
    = Target Signature
    -- creator: Signature.Creator of siblings that are to be created/removed.
    -- item: intended new item for the target.
    | Choose Item Creator
    | Modify Data Creator
    | Freeze


target : Signature -> Map Compositron
target sig =
    mark Node.passivate
        >> map_both ( Structure.find ( \this -> this.signature == sig ) |> perhaps )
        >> mark Node.activate

choose : Item -> Creator -> Map Compositron
choose future_item =
    manifest ( always future_item )
        
modify : Data -> Creator -> Map Compositron
modify dat =
    manifest ( Item.set_data dat )

freeze : Map Compositron
freeze =
    mark ( Node.map_item Item.freeze )

unfreeze : Map Compositron
unfreeze =
    mark ( Node.map_item Item.unfreeze )

        
create_intent :
    Creator
        -> Node
        -> Edit
        -> Intent Compositron
create_intent new_creator this edit =
    case edit of
        -- browse the actual tree.
        Target new_signature ->
            { serial = [ "🞋", Signature.serialize new_signature ] |> String.join (" ")
            , function = target new_signature
            , inverse = target ( this.signature )
            }
        -- switch the item to a more concrete type or a more ambiguous one.
        Choose new_item cre ->
            { serial = [ "!", Item.verbalize new_item ] |> String.join (" ")
            , function = choose new_item new_creator
            , inverse = choose ( this.item ) cre
            }
        -- switch the data in the item to be more concrete or a more empty.
        Modify new_data cre ->
            { serial = [ "~", Data.enstring new_data ] |> String.join (" ")
            , function = modify new_data new_creator 
            , inverse = modify ( Item.data this.item ) cre
            }
        -- make the UI render new data (workaround for contenteditable spans).
        Freeze ->
            { serial = [ "=", Data.enstring ( Item.data this.item ) ] |> String.join (" ")
            , function = freeze
            , inverse = unfreeze
            }

        

-- modify node

{-
map_signature : Map Signature -> Map Compositron
map_signature = Node.map_signature >> map_node
                
inc = map_signature ( Signature.inc )
      
map_item = Node.map_item >> map_node
set_item = always >> map_item
map_manifestation = Node.map_manifestation >> map_node 
-}
                          

    
-- preview

type alias Message msg m =
    { m | from_intent : Intent Compositron -> msg
        , from_command : Cmd msg -> msg
        , noop : msg
    }

preview :
    Creator
        -> Message msg m
        -> Compositron
        -> List ( Html msg )
           
preview new_creator message compositron =
    let
        incipit =
            Html.section []
                [ Html.h2
                      [ class "compositron", class "caption" ]
                      [ Html.text new_creator ]
                , Html.pre
                      []
                      [ Html.text ( serialize compositron ) ]
                ]
                    
        composition =
            live compositron
                |> Structure.tree
                |> view new_creator message
                |> View.present

    in [ incipit, composition ]

        
view :
    Creator
        -> Message msg m
        -> Branch
        -> View msg Item Signature Data
        
view new_creator message branch =
    let
        this : Node
        this = Structure.bode branch

        -- elements
                
        inner = \_->
            Structure.bildren branch
            |> List.map ( view new_creator message )

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
                Choose itm new_creator
                    |> to_message >> onClickNoBubble
            Input_url old ->
                ( Data.destring >> with new_creator Modify >> to_message )
                    |> onInput
            Input_span old ->
                Json.map
                    ( Data.destring >> with new_creator Modify >> to_message )
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
            create_intent new_creator this
            >> message.from_intent
               
    in
        View.basic "Compositron" this.signature inner
            |> Node.view this
            |> View.activate to_attribute
            


               

serialize : Compositron -> String
serialize = live >> Structure.serialize Node.serialize

deserialize : String -> Compositron
deserialize =
    Structure.deserialize Node.trivial Node.deserialize
        >> with trivial set_live

           
            
log = live >> Structure.log Node.serialize

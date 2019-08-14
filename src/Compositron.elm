module Compositron exposing
    ( State

    -- create
    , trivial

    -- view
    , preview

    -- serial form
    , log
    , serialize, deserialize

    -- testing
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
import Compositron.Signature as Signature exposing ( Creator )
import Compositron.Item as Item
import Compositron.Manifestation as Manifestation exposing ( Manifestation )
import Compositron.Data as Data exposing ( Data )

import Compositron.View as View exposing ( View, Action (..) )





-- Compositron


type alias State = Compositron
    
type Compositron =
    Compositron { live : LiveStructure, template : TempStructure }

type Domain
    = Live
    | Template
                
type alias LiveNode = Node.Node Live
type alias TempNode = Node.Node Template

type alias LiveSignature = Signature.Signature Live
type alias TempSignature = Signature.Signature Template
    
type alias LiveItem = Item.Item ( LiveSignature )
type alias TempItem = Item.Item ( TempSignature )

type alias LiveBranch = Structure.Branch LiveNode
type alias TempBranch = Structure.Branch TempNode
    
type alias LiveStructure = Structure.Structure LiveNode
type alias TempStructure = Structure.Structure TempNode
    
    
-- create

   
trivial : Compositron
trivial =
    Compositron
        { live = Structure.singleton ( Node.trivial )
        , template =
            Structure.deserialize """
▶template/0: Field: proxy: ⚠ former root
   template/1: Field: proxy: 🛈 Hoppla
     dupsel/0: ⚠ primer
"""
        }
 


--read


dereference : Compositron -> LiveSignature -> TempStructure
dereference comp sig =
    template comp
        |> perhaps ( Structure.find ( \this -> this.signature == sig_in_template sig ) )


live ( Compositron c ) = c.live
template ( Compositron c ) = c.template
                         
node = live >> Structure.node
template_node = template >> Structure.node

sig_in_template : LiveSignature -> TempSignature
sig_in_template = identity
              
form : Compositron -> LiveItem -> LiveForm 
form comp =
    let
        find_in_template : LiveSignature -> TempNode
        find_in_template =
            sig_in_template >> \t_sig ->
                template comp
                    |> perhaps ( Structure.find ( \this -> this.signature == t_sig ) )
                    |> Structure.node

        dereference : LiveSignature -> List LiveItem
        dereference sig =
            case find_in_template sig |> Node.map_domain Template Live of
                Assume _ |>
                
    in
        find_in_template
            >> \t_itm -> case t_itm of
                             Structure.kid_nodes
            >> List.map ( Node.map_domain Live Template >> .item )
            |> Item.form

            
signature = node >> .signature

item = node >> .item
       
manifestation =  node >> .manifestation

data = item >> Item.data

            

-- navigate


mark : Map LiveNode -> Map Compositron
mark =
    Structure.mark >> map_live 


        
-- map


map_both : Map ( Structure any ) -> Map Compositron
map_both fu = map_live fu >> map_template fu
    
map_live : Map LiveStructure -> Map Compositron
map_live fu ( Compositron c ) = Compositron { c | live = fu c.live }

set_live : LiveStructure -> Map Compositron
set_live = always >> map_live
                                
map_template : Map TempStructure -> Map Compositron
map_template fu ( Compositron c ) = Compositron { c | template = fu c.template }

                                    
                                       
-- modify


manifest : List LiveItem -> Creator -> Map Compositron
manifest new_items new_cre compositron =

    let
        
        -- Given a primer and a Compositron, perhaps impose a template and manifest it here
        satisfy : Map ( Node, Structure )
        satisfy ( pre, structure ) =

            let
                this_template : Maybe ( LZipper TempItem )
                this_template =
                    structure
                        |> Structure.node >> .item
                        |> Item.self_reference             -- Maybe ( LiveSignature )
                        |> Maybe.map
                           ( dereference                   -- Maybe ( TempStructure )
                               >> Structure.group_nodes )  -- ( [ TempNode ], TN, [ TempNode ] )
                           
                match_template_replacement : Maybe Node -> Item -> Skippable ( Maybe ( Map Node ) )
                match_template_replacement may_live temp_item =

                  case ( Maybe.map .item may_live, temp_item ) of
                    
                    ( Nothing, _ ) ->
                    -- more template items than live nodes, accept them:
                        Node.inc temp_item |> Just |> Match
                            
                    ( Just live_item, _ ) ->
                    -- compare a live item with a template item:
                        if live_item == temp_item

                    -- live match, discard the template item here:
                        then Match Nothing

                    -- template match, accept the template item here:
                        else Node.inc temp_item |> Just |> Match
                  
            in
                case this_template of
                    
                    Nothing ->    
                        ( pre, structure )
                            
                    Just template_group ->
                        ( pre, structure )

                        --(1) manifest the template item
                            |> Structure.tuck_items
                               [ template_group.focus ]
                               Node.inc
                                   
                        --(2) satisfy the neighbours
                            |> Structure.satisfy_template
                               ( template_group.before, template_group.after )
                               match_template_replacement

    in
        ( Node.primer new_cre, live compositron )

        --(1) manifest all new items
            |> Structure.tuck_items new_items Node.inc

        --(2) resatisfy assumptions in the group; repeat up to root.
            |> Structure.up_the_ancestry
                   ( Structure.each_in_group satisfy )

        -- back to Compositron, discard primer
            |> Tuple.second
            |> with compositron set_live



-- apply edit


type Edit
    = Target ( LiveSignature )
      
    -- creator: Signature.Creator of siblings that are to be created/removed.
    | Choose ( Nonempty TempSignature ) Creator
    | Modify Data Creator
      
    | Freeze


target : LiveSignature -> Map Compositron
target sig =
    mark Node.passivate
        >> map_live ( Structure.find ( \this -> this.signature == sig ) |> perhaps )
        >> mark Node.activate

choose : ( List TempSignature ) -> Creator -> Map Compositron
choose new_sigs cre comp =
    manifest
        ( new_sigs |> List.map ( dereference comp >> Structure.node >> .item >> always ) )
        cre comp

revert : Creator -> TempSignature -> Map Compositron
revert cre proto_ref comp =
    
        
modify : Data -> Creator -> Map Compositron
modify new_dat cre comp =
    manifest
        ( live comp |> Structure.node >> .item |> Item.set_data new_dat )
        cre comp

freeze : Map Compositron
freeze =
    mark ( Node.map_item Item.freeze )

unfreeze : Map Compositron
unfreeze =
    mark ( Node.map_item Item.unfreeze )

        
create_intent :
    Creator
        -> LiveNode
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
        -- switch the item to more concrete items, and revert them back to the more ambiguous one.
        Choose new_refs cre ->
            { serial = [ "!", Item.verbalize new_item ] |> String.join (" ")
            , function = choose new_refs new_creator
            , inverse = revert cre ( this.prototype )
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
                |> view_live_branch new_creator message
                |> View.present

    in [ incipit, composition ]

        
view_live_branch :
    Creator
        -> Message msg m
        -> LiveBranch
        -> View msg LiveItem ( LiveSignature ) Data
        
view_live_branch new_creator message branch =
    let
        this : LiveNode
        this = Structure.bode branch

               
        -- elements

        is_targeted =
            Manifestation.targeted this.manifestation

                            
        inner = \_->
            let
                alternatives_views =
                    if is_targeted then this.item |> Item.alternatives else []
                        |> List.map
                            ( \( r, efs ) ->
                               ( ( r, efs ), r |> dereference |> Structure.node |> .item ) )
                        |> List.map view_option

                view_option ( sigs, itm ) =
                    View.basic "option" this.signature ( always [] )
                        |> Item.view ( Item.info itm )
                        |> View.add_action ( Choose_this )
                        |> View.element ( always Html.button )
                        |> View.activate to_attribute    
        
            in Structure.bildren branch
                |> List.map ( view_live_branch new_creator message )
                |> (++) alternatives_views
            

        -- interactivity

        to_attribute :
            LiveSignature ->
            Action LiveItem Data ->
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
            Choose_this refs ->
                Choose refs new_creator
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
            

                                 
-- serial form
               

serialize : Compositron -> { live : String, template : String }
serialize c =
    { live = live c |> Structure.serialize Node.serialize
    , template = template c |> Structure.serialize Node.serialize
    }

deserialize : { live : String, template : String } -> Compositron
deserialize serial =
    { live = serial.live |> Structure.deserialize Node.trivial Node.deserialize
    , template = serial.template |> Structure.deserialize Node.trivial Node.deserialize
    }
           
            
log = live >> Structure.log Node.serialize

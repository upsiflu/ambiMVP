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


type alias State = Compositron
    
type Compositron =
    Compositron { live : LStructure, template : TempStructure }
                
type alias LiveNode = Node.Node Live
type alias TempNode = Node.Node Template

type alias LiveItem = Item.Item ( Signature Live )
type alias TempItem = Item.Item ( Signature Template )

type alias LiveBranch = Structure.Branch LiveNode
type alias TempBranch = Structure.Branch TempNode
    
type alias LiveStructure = Structure.Structure LiveNode
type alias TempStructure = Structure.Structure TempNode
    
type Domain
    = Live
    | Template
    
-- create


template : TempStructure
template = """
▶template/0: Field: proxy: ⚠ former root
   template/1: Field: proxy: 🛈 Hoppla
     dupsel/0: ⚠ primer
"""
    |> Structure.deserialize

   
trivial : Compositron
trivial =
    Compositron
        { live = Structure.singleton ( Node.trivial )
        , template = template
        }
 

        

-- mark the target node


mark : Map LiveNode -> Map Compositron
mark =
    Structure.mark >> map_live 


        
-- grow


{-- manifest: the first item to be instanciated.

 RULES.

 (1) signatures are unique, and creator matches the responsible transformation.
 (2) signatures are immutable: the target's signature remains untouched.
 (3) fields have their groups manifested as child nodes.
 (4) assumes have their template manifested in place.

 (5) 

--}

dereference : Signature Live -> Compositron -> TempStructure
dereference sig =
    template
        >> perhaps ( Structure.find ( \this -> this.signature == sig_in_template sig ) )


manifest : Map LiveItem -> Creator -> Map Compositron
manifest itm_fu new_cre compositron =

    let
        new_node = node compositron |> Node.map_item itm_fu

        -- Given a primer and a Compositron, perhaps impose a template and manifest it here
        satisfy : Map ( Node, Structure )
        satisfy ( pre, structure ) =

            let
                this_template : Maybe ( List TempItem, List TempItem )
                this_template =
                    structure
                        |> Structure.node >> .item
                        |> Item.self_reference             -- Maybe ( Signature Live )
                        |> Maybe.map
                           ( dereference                   -- Maybe ( TempStructure )
                               >> Structure.sibling_nodes  -- ( [ TempNode ], [ TempNode ] )
                               >> each ( List.map .item )  -- ( [ TempItem ], [ TempItem ] )
                           )
                
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
                case this_template of
                    Nothing ->
                        ( pre, structure )

                    Just template_neighbours ->
                        Structure.impose_template
                            template_neighbours
                            match_template_replacement
                            ( pre, structure )
                                   
    in
        ( Node.primer new_cre, live compositron )

        --(1) replace the current branch by a stub from the new item
            |> Structure.replace_branch
               ( new_node, [] )
               Node.inc

        --(2) reimpose desatisfied assumptions in the group, and do it up to root.
            |> Structure.up_the_ancestry
                   ( Structure.each_in_group satisfy )

        -- back to Compositron, discard primer
            |> Tuple.second
            |> with compositron set_live

               
-- read


live ( Compositron c ) = c.live
template ( Compositron c ) = c.template
                         
node = live >> Structure.node
template_node = template >> Structure.node

sig_in_template : Signature Live -> Signature Template
sig_in_template = identity
              
form : Compositron -> LiveItem -> LiveForm 
form comp =
    let
        find_in_template : Signature Live -> TempNode
        find_in_template =
            sig_in_template >> \t_sig ->
                template comp
                    |> perhaps ( Structure.find ( \this -> this.signature == t_sig ) )
                    |> Structure.node

        dereference : Signature Live -> List LiveItem
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




-- map


map_both : Map ( Structure any ) -> Map Compositron
map_both fu = map_live fu >> map_template fu
    
map_live : Map LiveStructure -> Map Compositron
map_live fu ( Compositron c ) = Compositron { c | live = fu c.live }

set_live : LiveStructure -> Map Compositron
set_live = always >> map_live
                                
map_template : Map TempStructure -> Map Compositron
map_template fu ( Compositron c ) = Compositron { c | template = fu c.template }


                                    
                                
-- apply edit


type Edit
    = Target ( Signature Live )
    -- creator: Signature.Creator of siblings that are to be created/removed.
    -- item: intended new item for the target.
    | Choose TempItem Creator
    | Modify Data Creator
    | Freeze


target : Signature Live -> Map Compositron
target sig =
    mark Node.passivate
        >> map_live ( Structure.find ( \this -> this.signature == sig ) |> perhaps )
        >> mark Node.activate

choose : TempItem -> Creator -> Map Compositron
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
        -> LiveBranch
        -> View msg LiveItem ( Signature Live ) Data
        
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
            Signature Live ->
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

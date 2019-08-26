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
    , modify
    )

{-| Compositron is a tree-shaped composition. It is comprised of a template and a live tree.
To edit the live tree, you can modify, choose, ... each node.

# Definition
@docs State

# Create
@docs trivial

# Edit
@docs choose
@docs target
@docs modify

# View and Interact
@docs preview

All interactions go through the rendered page. Since Compositron won't expose the functions that alter its state, we can only use serialize, deserialize and Edits to test it.

# Serial Form
@docs log
@docs serialize
@docs deserialize
-}





import Browser.Dom as Dom
import Html exposing ( Html )
import Html.Attributes exposing  ( .. )
import Html.Events exposing ( .. )
import Html.Lazy exposing ( .. )
import Json.Decode as Json
import Task


import History.Intent exposing ( Intent )

import Helpers exposing ( .. )
import Helpers.LZipper as LZipper exposing ( LZipper )

import Compositron.Structure.EagerZipperTree as Structure
import Compositron.Structure.Branch as Branch
import Compositron.Structure.Group as Group

import Compositron.Node as Node
import Compositron.Signature as Signature exposing ( Creator )
import Compositron.Item as Item
import Compositron.Manifestation as Manifestation exposing ( Manifestation )
import Compositron.Data as Data exposing ( Data )

import Compositron.View as View exposing ( View, Action (..) )





-- Compositron


{-| Comprises a live structure and a template structure. Use Edits to modify.-}
type alias State = Compositron


type Compositron =
    Compositron { live : LiveStructure, template : TempStructure }

type Live = Live
type Template = Template
type Prime = Prime

                
type alias LiveNode = Node.Node LiveSignature TempSignature
type alias TempNode = Node.Node TempSignature TempSignature
type alias PrimNode = Node.Node PrimSignature PrimSignature
    
type alias LiveSignature = Signature.Signature Live
type alias TempSignature = Signature.Signature Template
type alias PrimSignature = Signature.Signature Prime

type alias LiveBranch = Branch.Branch LiveNode
type alias TempBranch = Branch.Branch TempNode

type alias TempGroup = LZipper TempBranch
    
type alias LiveStructure = Structure.Structure LiveNode
type alias TempStructure = Structure.Structure TempNode

type alias LiveItem = Item.Item LiveSignature TempSignature
type alias TempItem = Item.Item TempSignature TempSignature

    
    
-- create

{-| A Compositron with a trivial live structure and an example program in template.
-}
trivial : State
trivial =
    deserialize
        { live = ""
        , template ="""
▶template/0: Field: proxy: ⚠ former root
   template/1: Field: proxy: 🛈 Hoppla
     dupsel/0: ⚠ primer
"""
        }
 


--read


dereference : State -> TempSignature -> Maybe TempStructure
dereference comp sig =
    template comp
        |> Structure.find ( \this -> this.signature == sig )


live ( Compositron c ) = c.live
template ( Compositron c ) = c.template

            

-- navigate


mark : Map LiveNode -> Map State
mark =
    Structure.mark >> map_live 


        
-- map


map_live : Map LiveStructure -> Map State
map_live fu ( Compositron c ) = Compositron { c | live = fu c.live }

set_live : LiveStructure -> Map State
set_live = always >> map_live
                                
map_template : Map TempStructure -> Map State
map_template fu ( Compositron c ) = Compositron { c | template = fu c.template }

                                    
                                       
-- modify


{-| 
--}
manifest : Creator -> Maybe TempGroup -> Map State
manifest new_cre new_tmp compositron =
    let
        next : PrimNode -> TempNode -> ( PrimNode, LiveNode )
        next p t =
            ( Node.inc p, Node.accept p t )

        match :
            LiveBranch ->
            TempBranch ->
            Match
        match liv tmp =
            if Branch.is Node.equal_items liv tmp then
                Keep
            else if ( Branch.node tmp |> .item |> Item.to_symbol ) /= Nothing then
                Skip
            else
                Insert
        
        satisfy : Map ( PrimNode, LiveStructure )
        satisfy ( pre, structure ) =
            let
                templates : LiveStructure -> Maybe TempGroup
                templates =
                    Structure.node
                        >> Node.self_reference
                        >> Maybe.andThen ( dereference compositron )
                        >> Maybe.map Structure.template_group
            in
                ( pre, structure )
                    |> maybe ( Structure.accept_template next match ) ( templates structure )

                     
    in
        ( Node.primer new_cre, live compositron )


        --(1) insert the 'before', 'focus', and 'after' branches, excepting the current node.
            |> maybe ( Structure.accept_template next match )
                     ( new_tmp )

        --(2) tuck the focused template *item*, if there is any, into the live structure.
            |> maybe ( Tuple.mapSecond
                           << Structure.mark << Node.set_item
                           << Item.accept << .item
                           << Branch.node << .focus
                     )
                     ( new_tmp )
                         
        --(3) resatisfy assumptions in the group; repeat down in each branch & up to root.
            |> Structure.up_and_down
                   ( Structure.each_in_group satisfy )

        -- back to State, discard primer
            |> Tuple.second
            |> with compositron set_live



-- apply edit


type Edit
    = Target ( LiveSignature )
      
    -- creator: Signature.Creator of siblings that are to be created/removed.
    | Choose Creator ( List TempSignature )
    | Modify Creator Data
      
    | Freeze

{-| Mark some node active.-}
target : LiveSignature -> Map State
target sig =
    mark Node.passivate
        >> map_live ( Structure.find ( \this -> this.signature == sig ) |> perhaps )
        >> mark Node.activate
        >> trace ("TARGET! "++Signature.serialize sig)

{-| disambiguate some node. -}
choose : Creator -> ( List TempSignature ) -> Map State
choose cre new_signatures compositron =
    let
        node = live compositron |> Structure.node
        available_signatures =
            Item.alternatives ( node.item )
                |> List.map ( Item.assumptions ( node.prototype ) )
    in
        if List.member new_signatures available_signatures then
            new_signatures
                |> List.reverse
                |> List.map ( dereference compositron )
                |> filter_just
                |> LZipper.from_list
                |> Maybe.map ( LZipper.map ( Structure.template_group ) >> LZipper.flatten )
                |> \new_branches -> manifest cre new_branches compositron
        else
            compositron
                |> trace
                     ( "Choice(s) "
                       ++(List.map Signature.serialize new_signatures |> String.join ", ")
                       ++" not available. Ignored. Possible choices: "
                       ++(List.map
                              ( List.map Signature.serialize >> String.join ", ")
                              available_signatures
                                  |> String.join " | "
                              )
                     )
               
{-| (TODO) undo any change done by a given Creator. -}
revert : Creator -> TempSignature -> Map State
revert cre proto_ref comp =
    comp
        
{-| change data in a node. -}
modify : Creator -> Data -> Map State
modify cre new_dat compositron =
    compositron
        |> mark ( Node.map_item ( Item.set_data new_dat ) )
        |> manifest cre Nothing
            

{-| make a node frozen. 
Nescessary to circumvent conflicts between HTML contenteditable property and the elm engine. -}
freeze : Map State
freeze =
    mark ( Node.map_item Item.freeze )

{-| before editing text, you need to taw data. -}
unfreeze : Map State
unfreeze =
    mark ( Node.map_item Item.unfreeze )

        
create_intent :
    Creator
        -> LiveNode
        -> Edit
        -> Intent State
create_intent new_creator this edit =
    case edit of
        -- browse the actual tree.
        Target new_signature ->
            { serial = [ "🞋", Signature.serialize new_signature ] |> String.join (" ")
            , function = target new_signature
            , inverse = target ( this.signature )
            }
        -- switch the item to more concrete items, and revert them back to the more ambiguous one.
        Choose cre new_refs ->
            { serial = [ "!", "some new references" ] |> String.join (" ")
            , function = choose new_creator new_refs
            , inverse = revert cre ( this.prototype )
            }
        -- switch the data in the item to be more concrete or a more empty.
        Modify cre new_data ->
            { serial = [ "~", Data.enstring new_data ] |> String.join (" ")
            , function = modify new_creator new_data 
            , inverse =
                case Item.data this.item of
                    Nothing -> identity
                    Just old_data -> modify cre old_data
            }
        -- make the UI render new data (workaround for contenteditable spans).
        Freeze ->
            { serial = [ "=", "current data" ] |> String.join (" ")
            , function = freeze
            , inverse = unfreeze
            }
                     

    
-- preview


type alias Message msg m =
    { m | from_intent : Intent State -> msg
        , from_command : Cmd msg -> msg
        , noop : msg
    }

{-| render an interactive tree. -}
preview :
    Creator
        -> Message msg m
        -> State
        -> List ( Html msg )
preview new_creator message compositron =
    let
        incipit =
            Html.section []
                [ Html.h2
                      [ class "compositron", class "caption" ]
                      [ Html.text new_creator ]
                , Html.details
                      []
                      [ Html.summary [] [ Html.figcaption [] [ Html.text "template..." ] ]
                      , Html.pre
                            []
                            [ Html.text ( serialize compositron |> .template )]
                      ]
                , Html.pre
                      []
                      [ Html.text ( serialize compositron |> .live )]
                ]
                
        composition =
            let
                show_symbol :
                    TempSignature ->
                        View msg LiveSignature TempSignature Data
                show_symbol =
                    dereference compositron
                        >> maybe
                               ( Structure.template_group
                                     >> Group.first ( .item >> Item.to_symbol )
                                     >> Maybe.withDefault ( Item.default_symbol )
                                     >> Item.accept
                                     >> Item.view
                               )
                        >> over ( View.static "Template" )
            in
                live compositron
                    |> Structure.tree
                    |> view_live_branch
                           new_creator
                           message
                           show_symbol
                    |> View.present
    in
        [ incipit, composition ]


        
view_live_branch :
    Creator
        -> Message msg m
        -> ( TempSignature -> View msg LiveSignature TempSignature Data )
        -> LiveBranch
        -> View msg LiveSignature TempSignature Data
        
view_live_branch new_creator message view_template_branch branch =
    let
        this : LiveNode
        this = Branch.node branch

        is_targeted =
            Manifestation.targeted this.manifestation
                            
        inner = \_->
            let
                alternative_cogroup_views =
                    ( if is_targeted then Item.alternatives this.item else [] )
                        |> List.map
                           ( Item.view_cogroup this.prototype view_template_branch )
                        |> List.indexedMap
                           ( \n ->
                                over ( View.ephemeral "Option" ( Signature.ephemeral n ) )
                                     >> View.element ( always Html.button )
                                     >> View.activate to_attribute
                           )
            in
                Branch.kids branch
                |> List.map ( view_live_branch new_creator message view_template_branch )
                |> (++) alternative_cogroup_views
            

        -- interactivity

        to_attribute :
            LiveSignature ->
            Action TempSignature Data ->
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
            Choose_these refs ->
                Choose new_creator refs
                    |> to_message >> onClickNoBubble
            Input_url old ->
                ( Data.destring >> Modify new_creator >> to_message )
                    |> onInput
            Input_span old ->
                Json.map
                    ( Data.destring >> Modify new_creator >> to_message )
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
            -->> Debug.log "message created"

    in
        View.active "State" this.signature inner
            |> Node.view this
            |> View.activate to_attribute
            

                                 
-- serial form
               

{-| Unique String representation, given an appropriate node serializer.-}
serialize : State -> { live : String, template : String }
serialize c =
    { live = live c |> Structure.serialize Node.serialize
    , template = template c |> Structure.serialize Node.serialize
    }

{-| Parse from a unique string, given an appropriate node deserializer. -}
deserialize : { live : String, template : String } -> State
deserialize serial =
    Compositron
    { live = serial.live
        |> Structure.deserialize
              ( Node.deserialize >> Maybe.withDefault Node.error >> Node.adopt_domain )
              ( .manifestation >> Manifestation.targeted )
    , template = serial.template
        |> Structure.deserialize
              ( Node.deserialize >> Maybe.withDefault Node.error )
              ( .manifestation >> Manifestation.targeted )
    }
           
            
{-| Console output. -}
log : State -> LiveStructure
log = live >> Structure.log Node.serialize

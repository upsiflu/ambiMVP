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
import Compositron.Manifestation as Manifestation exposing ( Manifestation (..) )
import Compositron.Data as Data exposing ( Data )
import Compositron.Cogroup as Cogroup

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
target sig compositron =
    case
        live compositron  |> Structure.find ( .signature >> (==) sig )
    of
        Nothing ->
            compositron 
        Just new ->
            compositron
                |> mark ( Node.set_manifestation Notargeted )
                |> map_live ( perhaps ( Structure.find ( .signature >> (==) sig ) ) )
                |> mark ( Node.set_manifestation Targeted )


                
{-| disambiguate some node. -}
choose : Creator -> ( List TempSignature ) -> Map State
choose cre new_signatures compositron =
    let
        node = live compositron |> Structure.node
        available_signatures =
            Item.alternatives ( node.item )
                |> List.map ( Cogroup.assumptions ( node.prototype ) )
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
            { serial = [ "~", Data.serialize new_data |> Debug.log "=" ] |> String.join (" ")
            , function = modify new_creator new_data |> Debug.log "executing Modify" 
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



      
    
-- view


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

        -- interactivity

        to_choice :
            LiveNode
            -> List TempSignature
            -> { face : String, choice : Html.Attribute msg }
        to_choice this protos =
            { face = List.head protos
                  |> Maybe.andThen ( dereference compositron )
                  |> Maybe.withDefault ( template compositron )
                  |> Structure.template_group
                  |> Group.first ( .item >> Item.option_face )
                  |> Maybe.withDefault ( Item.default_face )
            
            , choice = protos
                  --|> Debug.log "choose these on click"
                  |> Choose new_creator
                  |> to_message this >> onClickNoBubble
            }
            
        to_message :
            LiveNode
            -> Edit
            -> msg
        to_message this =
            create_intent new_creator this
                >> message.from_intent
            
        to_attribute :
            LiveNode ->
            Action ->
            Html.Attribute msg
        to_attribute this act =
            let
                merge_data : String -> Data
                merge_data str =
                    this.item |> Item.data |> Data.merge str
            in
               case act of
                   Id str ->
                       Html.Attributes.id str
                   Class str ->
                       Html.Attributes.class str
                   Navigate_here -> 
                       Target this.signature
                           |> to_message this >> onClickNoBubble
                   Focus_here ->
                       Dom.focus ( Signature.serialize this.signature )
                           |> Task.attempt (\_-> message.noop )
                           |> message.from_command >> onClickNoBubble
                   Input_span old ->
                       Json.map
                           ( merge_data >> Modify new_creator >> to_message this )
                           ( Json.at [ "detail" ] Json.string )
                           |> Debug.log "on string changed..."
                           |> on "stringChanged"
                   Input_url old ->
                       ( merge_data >> Modify new_creator >> to_message this )
                           |> onInput
                   Blur_span ->
                       Freeze
                           |> to_message this
                           |> onBlur
                   Contenteditable ->
                       class "" --Html.Attributes.contenteditable False
                   When_targeted action ->
                       if Manifestation.targeted this.manifestation
                       then to_attribute this action
                       else class ""
                           

        view_live_branch :
            LiveBranch
            -> View LiveNode TempSignature
        view_live_branch branch =
            View.live ( Branch.node branch )
                |> Node.view ( Branch.node branch )
                |> View.kids ( Branch.kids branch |> List.map view_live_branch )

                
        composition =
            live compositron
                |> Structure.tree
                |> view_live_branch
                |> View.view { to_choice = to_choice, to_attribute = to_attribute }
    in
        [ incipit, composition ]            

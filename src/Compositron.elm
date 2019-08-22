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

type Live = Live
type Template = Template
type Prime = Prime

                
type alias LiveNode = Node.Node LiveSignature TempSignature
type alias TempNode = Node.Node TempSignature TempSignature
type alias PrimNode = Node.Node PrimSignature PrimSignature
    
type alias LiveSignature = Signature.Signature Live
type alias TempSignature = Signature.Signature Template
type alias PrimSignature = Signature.Signature Prime

type alias LiveBranch = Structure.Branch LiveNode
type alias TempBranch = Structure.Branch TempNode
    
type alias LiveStructure = Structure.Structure LiveNode
type alias TempStructure = Structure.Structure TempNode

type alias LiveItem = Item.Item LiveSignature TempSignature
type alias TempItem = Item.Item TempSignature TempSignature

    
    
-- create

   
trivial : Compositron
trivial =
    Compositron
        { live = Structure.singleton ( Node.trivial |> Node.map_domain )
        , template =
            Structure.deserialize
                Node.trivial
                Node.deserialize
                """
▶template/0: Field: proxy: ⚠ former root
   template/1: Field: proxy: 🛈 Hoppla
     dupsel/0: ⚠ primer
"""
        }
 


--read


dereference : Compositron -> TempSignature -> Maybe TempStructure
dereference comp sig =
    template comp
        |> Structure.find ( \this -> this.signature == sig )


live ( Compositron c ) = c.live
template ( Compositron c ) = c.template
                         
node = live >> Structure.node
template_node = template >> Structure.node

signature = node >> .signature

item = node >> .item
       
manifestation =  node >> .manifestation

data = item >> Item.data

            

-- navigate


mark : Map LiveNode -> Map Compositron
mark =
    Structure.mark >> map_live 


        
-- map


map_live : Map LiveStructure -> Map Compositron
map_live fu ( Compositron c ) = Compositron { c | live = fu c.live }

set_live : LiveStructure -> Map Compositron
set_live = always >> map_live
                                
map_template : Map TempStructure -> Map Compositron
map_template fu ( Compositron c ) = Compositron { c | template = fu c.template }

                                    
                                       
-- modify


manifest : Creator -> Maybe ( Nonempty TempBranch ) -> Map Compositron
manifest new_cre new_branches compositron =

    let         
        match_template_replacement :
            Maybe LiveBranch ->
            TempBranch ->
                Match ( PrimNode, TempBranch ) ( PrimNode, LiveBranch )
        match_template_replacement may_live tmp =
            case may_live of
                    
                Nothing ->
                    -- more template items than live nodes, accept them:
                    Fix <| Structure.accept_branch Node.inc Node.accept
                                   
                Just liv ->
                    if Structure.equal_branches Node.equal_items liv tmp

                    -- live matches. Discard the template branch here:
                        then Pursue

                    -- template differs. Accept the template branch here:
                        else
                            Fix <| Structure.accept_branch Node.inc Node.accept
        
        -- Given a primer and a Compositron, perhaps impose a template and manifest it here
        satisfy : Map ( PrimNode, LiveStructure )
        satisfy ( pre, structure ) =

            let
                this_template : Maybe ( LZipper TempBranch )
                this_template =
                    structure
                        |> Structure.node
                        |> Node.self_reference             -- Maybe ( LiveSignature )
                        |> Maybe.andThen ( dereference compositron )-- Maybe ( TempStructure )
                        |> Maybe.map Structure.group_branches -- ( [ TempB ], TempB, [ TempB ] )
                                
            in
                case this_template of
                    
                    Nothing ->
                        ( pre, structure )
                            
                    Just template_group ->
                        ( pre, structure )
                            |> Structure.accept_template
                                   template_group
                                   match_template_replacement

        accept_new_branches =
            case new_branches of
                Nothing ->
                    identity
                Just branches ->
                    Structure.accept_template
                        ( branches |> nonempty_to_lzipper )
                        match_template_replacement

    in
        ( Node.primer new_cre, live compositron )

        --(1) manifest new branches if there are any
            |> accept_new_branches
               
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
    | Choose Creator ( List TempSignature )
    | Modify Creator Data
      
    | Freeze


target : LiveSignature -> Map Compositron
target sig =
    mark Node.passivate
        >> map_live ( Structure.find ( \this -> this.signature == sig ) |> perhaps )
        >> mark Node.activate

choose : Creator -> ( List TempSignature ) -> Map Compositron
choose cre new_signatures compositron =
    new_signatures
        |> List.map ( dereference compositron )
        |> filter_just
        |> List.map Structure.branch
        |> to_nonempty
        |> \new_branches -> manifest cre new_branches compositron
       
revert : Creator -> TempSignature -> Map Compositron
revert cre proto_ref comp =
    comp
        
modify : Creator -> Data -> Map Compositron
modify cre new_dat compositron =
    compositron
        |> mark ( Node.map_item ( Item.set_data new_dat ) )
        |> manifest cre Nothing
            

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
                      [ Html.text ( serialize compositron |> .live )
                      , Html.br [] []
                      , Html.text ( serialize compositron |> .template )
                      ]
                ]
                
        composition =
            let
                view_template_branch :
                    TempSignature ->
                        View msg LiveSignature TempSignature Data
                view_template_branch =
                    dereference compositron
                        >> Maybe.map Structure.node
                        >> Maybe.map ( .item >> Item.accept >> Item.view )
                        >> Maybe.withDefault identity
                        >> over ( View.static "Template" )
            in
                live compositron
                    |> Structure.tree
                    |> view_live_branch
                           new_creator
                           message
                           view_template_branch
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
        this = Structure.bode branch

               
        -- elements

        is_targeted =
            Manifestation.targeted this.manifestation

                            
        inner = \_->
            let
                alternative_cogroup_views =
                    ( if is_targeted then Item.alternatives this.item else [] )
                        |> List.map
                           ( Item.view_cogroup this.prototype view_template_branch )
                                       -->> View.add_action ( Choose_this () )
                        |> List.map
                           (\cog -> View.ephemeral "Option" Signature.ephemeral
                               |> cog 
                               |> View.element ( always Html.button )
                               |> View.activate to_attribute
                           )
            in
                Structure.bildren branch
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

               
    in
        View.active "Compositron" this.signature inner
            |> Node.view this
            |> View.activate to_attribute
            

                                 
-- serial form
               

serialize : Compositron -> { live : String, template : String }
serialize c =
    { live = live c |> Structure.serialize Node.serialize
    , template = template c |> Structure.serialize Node.serialize
    }

deserialize : { live : String, template : String } ->
              { live : LiveStructure
              , template : TempStructure
              }
deserialize serial =
    { live = serial.live
        |> Structure.deserialize
              ( Node.trivial |> Node.map_domain )
              ( Node.deserialize >> Maybe.map Node.map_domain)
    , template = serial.template
        |> Structure.deserialize
              ( Node.trivial )
              ( Node.deserialize )
    }
           
            
log = live >> Structure.log Node.serialize

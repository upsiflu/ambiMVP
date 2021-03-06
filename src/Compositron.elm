module Compositron exposing
    ( State

    -- create
    , trivial

    -- view
    , view

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
@docs view

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
import Helpers.Nonempty as Nonempty exposing ( Nonempty )

import Compositron.Structure.EagerZipperTree as Structure
import Compositron.Structure.Branch as Branch
import Compositron.Structure.Group as Group

import Compositron.Node as Node
import Compositron.Signature as Signature exposing ( Creator )
import Compositron.Item as Item
import Compositron.Manifestation as Manifestation exposing ( Manifestation (..) )
import Compositron.Data as Data exposing ( Data )
import Compositron.Cogroup as Cogroup exposing ( Cogroup (..) )

import Compositron.View as View exposing ( View, Action (..) )





-- Compositron


{-| Comprises a live structure and a template structure. Use Edits to modify.-}
type alias State = Compositron


type Compositron =
    Compositron
        { live : LiveStructure
        , template : TempStructure
        , reference : TempStructure
        }

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
        , template = ""
        , reference = ""
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

           
                                    
                                       
-- modify


{-| Given a new creator and a Group from the template domain, manifest replaces the current target with the focus of the Group (via item replacement; other node properties are preserved), and matches the neighbours progressively through a comparison function _match_.
-}
manifest : Creator -> Maybe TempGroup -> Map State
manifest new_cre cogroup compositron =
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

        accept_item =
            Tuple.mapSecond
                << Structure.mark << Node.set_item
                << Item.accept << .item
                << Branch.node << .focus
        
        satisfy : Map ( PrimNode, LiveStructure )
        satisfy ( pre, structure ) =
            let
                templates : LiveStructure -> Maybe TempGroup
                templates =
                    Structure.node
                        >> Node.reference
                        >> Maybe.andThen ( dereference compositron )
                        >> Maybe.map Structure.template_group
            in
                ( pre, structure )
                    |> maybe ( Structure.accept_template next match ) ( templates structure )
                    |> maybe ( accept_item ) ( templates structure )
                       {- PROBLEM:
                          the non-self assumption is not replaced by what it assumes
                          so that its template is added over and over.
                          SOLUTION:
                          satisfy satisfiable assumptions in a template that is to be manifested.
                          -} 
                     
    in
        ( Node.primer new_cre, live compositron )


        --(1) insert the 'before', 'focus', and 'after' branches, excepting the current node.
            |> maybe ( Structure.accept_template next match ) new_tmp

        --(2) tuck the focused template *item*, if there is any, into the live structure.
            |> maybe accept_item new_tmp
                         
        --(3) resatisfy assumptions in the group; repeat down in each branch & up to root.
            |> Structure.up_and_down satisfy

        -- back to State, discard primer
            |> Tuple.second
            |> with compositron set_live



-- apply edit


type Edit
    = Target ( LiveSignature )
      
    -- creator: Signature.Creator of siblings that are to be created/removed.
    | Choose Creator ( Cogroup TempSignature )
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
choose : Creator -> Cogroup TempSignature -> Map State
choose cre cogroup compositron =
    let
        node = live compositron |> Structure.node
        cogroup_available =
            Item.options ( node.item )
                |> Maybe.map ( Nonempty.member cogroup )
                |> Maybe.withDefault False
    in
        if cogroup_available then
            cogroup
                |> Cogroup.arrows
                |> List.reverse
                |> List.map ( Arrow.to_reference node.prototype >> dereference compositron )
                |> filter_just
                |> LZipper.from_list
                |> Maybe.map ( LZipper.map ( Structure.template_group ) >> LZipper.flatten )
                |> \new_branches -> manifest cre new_branches compositron
        else
            compositron
                |> trace
                     ( "Choice(s) "
                       ++ ( Nonempty.map Signature.serialize new_signatures |> Nonempty.to_list |> String.join ", " )
                       ++ " not available. Ignored." )
               
{-| (TODO) undo any change done by a given Creator. -}
revert : Creator -> TempSignature -> Map State
revert cre proto_ref comp =
    comp
        
{-| change data in a node. -}
modify : Creator -> Data -> Map State
modify cre new_dat compositron =
    compositron
        |> mark ( Node.map_item ( Item.map_data ( \_->new_dat ) ) )
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
    , template = template c |> .unsatisfied |> Structure.serialize Node.serialize
    }

{-| Parse from a unique string, given an appropriate node deserializer. -}
deserialize : { live : String, template : { unsatisfied : String } } -> State
deserialize serial =
    Compositron
    { live = serial.live
        |> Structure.deserialize
              ( Node.deserialize >> Maybe.withDefault Node.error >> Node.adopt_domain )
              ( .manifestation >> Manifestation.targeted )
    , template = serial.template.unsatisfied
        |> Structure.deserialize
              ( Node.deserialize >> Maybe.withDefault Node.error )
              ( .manifestation >> Manifestation.targeted )
        |> both ( identity, Structure.satisfy )
        |> \( u, s ) -> { unsatisfied = u, satisfied = s }
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
view :
    Creator
        -> Message msg m
        -> State
        -> List ( Html msg )
view new_creator message compositron =
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
            -> Nonempty TempSignature
            -> { face : String, choice : Html.Attribute msg }
        to_choice this ( pro, tos ) =
            { face = pro
                  |> dereference compositron
                  |> Maybe.withDefault ( template compositron )
                  |> Structure.template_group
                  |> Group.first ( .item >> Item.option_face )
                  |> Maybe.withDefault ( "*?*" )
            
            , choice =( pro, tos )
                  --|> Debug.log "choose these on click"
                  |> Choose new_creator
                  |> to_message this
                  >> onClick
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
                   Tab_here ->
                       tabindex 0
                   Target_here ->
                       if not ( Manifestation.targeted this.manifestation )
                       then Target this.signature
                           |> to_message this >> onPressNoBubble
                       else class "T"
                   Focus_here ->
                       Dom.focus ( Signature.serialize this.signature ++ "-input"
                                 |> Debug.log "focus here" )
                           |> Task.attempt (\_-> message.noop )
                           |> message.from_command >> onPressNoBubble
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
                   Property string decoder ->
                       property string decoder
                           

        view_live_branch :
            LiveBranch
            -> View LiveNode TempSignature
        view_live_branch branch =
            Node.view ( Branch.node branch )
                |> case Branch.kids branch |> List.map view_live_branch |> Nonempty.from_list of
                       Nothing -> identity
                       Just kidz -> View.contain ( kidz )
    in
        [ incipit
        , live compositron
            |> Structure.tree >> view_live_branch
            |> View.view { to_choice = to_choice, to_attribute = to_attribute }
        ] 

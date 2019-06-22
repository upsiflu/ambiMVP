module Compositron exposing
    ( State, trivial, possible_intents, preview )

import Tree exposing ( Tree )
import Tree.Zipper exposing  ( Zipper, mapLabel, root, tree, forward, append, findFromRoot, fromTree )
import Tuple exposing ( first, second )

import Html exposing ( .. )
import Html.Attributes exposing  ( .. )
import Html.Events exposing ( .. )

import Compositron.View as View exposing ( View )
import History.Intent exposing ( Intent )

import Helpers exposing ( .. )



-- Compositron

type alias State = Compositron

type alias Node = { signature : Signature, item : Item }

    
type alias Compositron = Zipper Node
type alias Compositree = Tree Node

type alias Signature = String
      
type Item

    -- kinds:
    = Style ( Maybe String )
    | Span ( Maybe String )
    | Parag
    
    -- automatically inserted:
    | Ambiguous
      
    -- only for producing Html:
    | Highlight Item

type alias Endofunction =
    Compositron -> Compositron



                   
-- create


trivial : Compositron
trivial =
    Tree.singleton { signature = "start", item = Ambiguous }
        |> fromTree
        |> (\_-> alternative )



alternative =
    Tree.tree { signature = "0", item = Parag }
        [ Tree.singleton { signature = "0.0", item = Ambiguous }
        , Tree.tree      { signature = "0.1", item = Span ( Just "A b c " ) }
            [ Tree.singleton { signature = "0.1.0", item = Ambiguous } ]
        , Tree.tree      { signature = "0.2", item = Style ( Just "align-right" ) }
            [ Tree.singleton { signature = "0.2.0", item = Ambiguous }
            , Tree.tree      { signature = "0.2.1", item = Span ( Just "(align-right) D e f" ) }
                [ Tree.singleton { signature = "0.2.1.0", item = Ambiguous } ]  
            , Tree.tree      { signature = "0.2.2", item = Span ( Just "G h i" ) } 
                [ Tree.singleton { signature = "0.2.2.0", item = Ambiguous } ] 
            ]
        , Tree.tree { signature = "0.3", item = Style Nothing }
            [ Tree.singleton { signature = "0.3.0", item = Ambiguous }  
            , Tree.tree      { signature = "0.3.1", item = Span ( Just "(empty style) J k l" ) }
                [ Tree.singleton { signature = "0.3.1.0", item = Ambiguous } ] 
            ]
        ]
        |> fromTree



-- read


signature = Tree.Zipper.label >> .signature
item = Tree.Zipper.label >> .item


       
-- map


type alias Map = Compositron -> Compositron

target : Signature -> Map
target sig =
    findFromRoot ( \node -> .signature node == sig )
    >> Maybe.withDefault trivial

untarget : Signature -> Map
untarget sig =
    root

set : ( Maybe String, Maybe String ) -> Map
set ( string, old ) =
    mapLabel ( \node -> { node | item = 
        case node.item of
            Span _ -> Span string
            Style _ -> Style string
            x -> x
                        } )

choose : Signature -> Item -> Map
choose new choice =
    logTree
    >> Tree.Zipper.replaceTree 
        ( Tree.tree 
            { signature = new, item = choice }
            [ Tree.singleton { signature = new++"inside", item = Ambiguous } ] 
        ) 
    >> logTree
    >> addNode 
        { signature = new++"before", item = Ambiguous } 
    >> logTree

logTree = Debug.log "tree"
    
addNode : Node -> Map
addNode n z =
    case Tree.Zipper.parent z of
        Nothing -> z
        Just p -> p |>
           Tree.Zipper.mapTree
                ( \t -> Tree.mapChildren ( \c-> ( Tree.singleton n ) :: c ) t ) 
                
clear : Item -> Map
clear choice =
    mapLabel ( \node -> { node | item = Ambiguous } ) 


    
-- preview

preview :
    Signature
        -> ( Intent Compositron -> msg )
        -> Compositron
        -> List ( Html msg )
           
preview sig intent_to_message compositron =
    let
        click =
            create_intent sig compositron
            >> intent_to_message
            >> onClickNoBubble

        navigate_back =
            Untarget ( signature compositron )
            |> click

        incipit =
            h2 [ class "compositron", class "caption" ] [ text sig ]
          
        composition =
            compositron
                |> mapLabel ( \node -> { node | item = Highlight node.item } )
                |> both ( root >> tree, identity )
                |> toHtml sig intent_to_message

    in incipit :: composition

        
       
toHtml :
    Signature
        -> ( Intent Compositron -> msg )
        -> ( Compositree, Compositron )
        -> List ( Html msg )
        
toHtml sig intent_to_message structure =
    let
        ( compositree, compositron ) =
            structure
                
        node =
            Tree.label compositree

        inner = \_->
            Tree.children compositree
            |> List.map (\child -> toHtml sig intent_to_message ( child, compositron ) )
            |> List.concat

        to_message =
            create_intent sig compositron
            >> intent_to_message

        navigate_here =
            Target node.signature
            |> to_message >> onClickNoBubble
               
        choose_this option =
            Choose option
            |> to_message >> onClickNoBubble

        set_this old =
            ( \new -> to_message ( Set ( ( destring new ), old ) ) )
            |> onInput

        destring string =
            case string of
                 ""  -> Nothing
                 str -> Just str

        default_view = View.default node.signature navigate_here ( inner () )
                       
        view_style string =
            default_view "Style"
                |> View.set_icon ( text "S" )
                |> View.map_anchor
                    identity
                    ( input
                        [ id ( node.signature ++ "-input" )
                        , class "input"
                        , value ( string |> Maybe.withDefault "" )
                        , set_this string
                        ] [] |> (::)
                    )
        
        view_span string =
            default_view "Span"
                |> View.set_icon ( text "T" )
                |> View.map_anchor
                    identity
                    ( input
                        [ name ( node.signature ++ "-input" )
                        , class "input"
                        , value ( string |> Maybe.withDefault "" )
                        , set_this string
                        ] [] |> (::)
                    )
                |> View.map_item span
                    ( for ( node.signature ++ "-input" ) |> (::) )
                    ( text ( string |> Maybe.withDefault "" ) |> (::) )

        view_parag =
            default_view "Parag"
                |> View.set_icon ( text "P" )
                |> View.map_anchor
                     identity
                     ( label [ class "symbol" ] [ text "P" ] |> (::) )
                |> View.map_item p
                     identity
                     identity
                         
        view_ambiguous =
            default_view "Ambiguous"
                |> View.set_icon ( text "+" )
                |> View.map_anchor
                      ( class "ellipsis" |> (::) ) 
                      ( [ text "+" ] |> always )         

        
        
    in  case node.item
        of
        Style string ->
            view_style string |> View.present
            
        Span string ->
            view_span string |> View.present
                
        Parag ->
            view_parag |> View.present

        Ambiguous ->
            view_ambiguous |> View.present
            
        Highlight i ->
           case i
            of
            
            Style string ->
                view_style string |> View.interactive |> View.present
                
            Span string ->
                view_span string |> View.interactive |> View.present
                
            Parag ->
                view_parag |> View.interactive |> View.present
                      
            Ambiguous ->
                view_ambiguous
                |> View.interactive
                |> View.map_anchor
                      identity
                      ( label
                        []
                        [ button [ class "choice", choose_this ( Style Nothing ) ]
                                 [ text "Style" ]
                        , button [ class "choice", choose_this ( Span Nothing ) ]
                                 [ text "Span" ]
                        , button [ class "choice", choose_this Parag ]
                                 [ text "Paragraph" ]
                        ] |> ( before [] ) |> always
                      )
                |> View.present
                
            _ -> [text ";"]
                                                        



                 
-- edits

allNodes = tree >> Tree.flatten
           
allTargets = allNodes >> List.map ( \node -> Target node.signature )
           
allEdits = allTargets
    
type Edit
    = Target String
    | Untarget String
    | Choose Item
    | Clear Item
    | Set ( Maybe String, Maybe String )


possible_intents :
    Signature
        -> Compositron
        -> List ( Intent Compositron )
possible_intents sig compositron =
    List.map
        ( \edit -> create_intent sig compositron edit )
        ( allEdits compositron )
            

create_intent :
    Signature
        -> Compositron
        -> Edit
        -> Intent Compositron
create_intent sig compositron edit =
    let
        verbalize choice =
            case choice of
                Span _ -> "V"
                Parag -> "P"
                Style _ -> "S"
                _ -> "weird"
        normalize_str =
            Maybe.withDefault ""
                
    in case edit of
        Target t ->
            { serial = "🞋 " ++t++" ("++( signature compositron )++")"
            , function = target t
            , inverse = target ( signature compositron )
            }
        Untarget t ->
            { serial = "×"
            , function = untarget t
            , inverse = target t
            }
        Choose choice ->
            { serial = "! "++ ( verbalize choice )
            , function = choose sig choice
            , inverse = choose sig ( item compositron )
            }
        Clear choice ->
            { serial = "- "++ ( verbalize choice )
            , function = choose sig Ambiguous
            , inverse = choose sig choice
            }
        Set ( string, old ) ->
            { serial = "="++ ( normalize_str string ) ++ ", " ++ ( normalize_str old )
            , function = set ( string, old )
            , inverse = set ( old, string )
            }


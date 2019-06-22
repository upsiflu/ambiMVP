module Compositron exposing
    ( State, trivial, possible_intents, preview )

import Tree exposing ( Tree )
import Tree.Zipper exposing  ( Zipper, mapLabel, root, tree, forward, append, findFromRoot, fromTree )
import Tuple exposing ( first, second )

import Html exposing ( .. )
import Html.Attributes exposing  ( .. )
import Html.Events exposing ( .. )

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
            
            
    -- layout
    
    {- every node consists of
        - an anchor (probably a button)
        - perhaps a presentation
        - and a container of its children.
     -}
     
        no_html = text ""
        default_view item_type =
            { item_type = item_type
            , item_icon = "?"
            , attributes = 
                [ class item_type 
                , class "anchor"
                , navigate_here
                ]
            , on_anchor = no_html
            , on_item = no_html
            , subsequent = inner
            }

        interactive params =
            { params | attributes = ( class "targeted" )::params.attributes }

        with_on_anchor a params = { params | on_anchor = a }
        with_on_item i params = { params | on_item = i }
        with_icon i params = { params | item_icon = i }
        with_subsequent s params = { params | subsequent = s }
        
        present view_parameters =
            button
                view_parameters.attributes
                [ label [ class "signature" ] [ text node.signature ]
                , label [ class "item_icon" ] [ text view_parameters.item_icon ]
                , label [ class "item_type" ] [ text view_parameters.item_type ]
                , view_parameters.on_anchor
                ]
            |> before ( view_parameters.on_item :: ( view_parameters.subsequent () ) )

        view_style string =
            default_view "Style"
                |> with_icon "S"
                |> with_on_anchor
                    ( input
                        [ id ( node.signature ++ "-input" )
                        , class "input"
                        , value ( string |> Maybe.withDefault "" )
                        , set_this string
                        ] []
                    )
        
        view_span string =
            default_view "Span"
                |> with_icon "T"
                |> with_on_anchor
                    ( input
                        [ id ( node.signature ++ "-input" )
                        , class "input"
                        , value ( string |> Maybe.withDefault "" )
                        , set_this string
                        ] []
                    )
                |> with_on_item
                    ( span
                        [ id ( node.signature ++ "-value" )
                        ] [ text ( string |> Maybe.withDefault "" ) ]
                    )

        view_parag =
            default_view "Parag"
                |> with_icon "P"
                |> with_on_anchor ( label [ class "symbol" ] [ text "P" ] )
                |> with_subsequent ( always [ p [] ( inner () ) ] )
                
        view_ambiguous =
            default_view "Ambiguous"
                |> with_icon "+"
                |> with_on_anchor
                    ( button 
                        [ class "ellipsis" ] 
                        [ text "+" ]
                    )
            

        
        
    in  case node.item
        of
        Style string ->
            view_style string |> present
            
        Span string ->
            view_span string |> present
                
        Parag ->
            view_parag |> present

        Ambiguous ->
            view_ambiguous |> present
            
        Highlight i ->
           case i
            of
            
            Style string ->
                view_style string |> interactive |> present
                
            Span string ->
                view_span string |> interactive |> present
                
            Parag ->
                view_parag |> interactive |> present
                      
            Ambiguous ->
                view_ambiguous
                |> interactive >> with_on_anchor
                    ( label []
                        [ button [ class "choice", choose_this ( Style Nothing ) ]
                                 [ text "Style" ]
                        , button [ class "choice", choose_this ( Span Nothing ) ]
                                 [ text "Span" ]
                        , button [ class "choice", choose_this Parag ]
                                 [ text "Paragraph" ]
                        ]
                    )
                |> present
                
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


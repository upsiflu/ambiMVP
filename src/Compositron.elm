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
    = Style
    | Value ( Maybe String )
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



alternative =
    Tree.tree { signature = "0", item = Parag }
        [ Tree.singleton { signature = "0.0", item = Ambiguous }
        , Tree.tree      { signature = "0.1", item = Value ( Just "paragraph text" ) }
            [ Tree.singleton { signature = "0.1.0", item = Ambiguous } ]
        , Tree.tree      { signature = "0.2", item = Style }
            [ Tree.singleton { signature = "0.2.0", item = Ambiguous }
            , Tree.tree      { signature = "0.2.1", item = Value ( Just "align left" ) }
                [ Tree.singleton { signature = "0.2.1.0", item = Ambiguous } ]  
            , Tree.tree      { signature = "0.2.2", item = Value ( Just "red shade" ) } 
                [ Tree.singleton { signature = "0.2.2.0", item = Ambiguous } ] 
            ]
        , Tree.tree { signature = "0.3", item = Style }
            [ Tree.singleton { signature = "0.3.0", item = Ambiguous }  
            , Tree.tree      { signature = "0.3.1", item = Value ( Just "blue shade" ) }
                [ Tree.singleton { signature = "0.3.1.0", item = Ambiguous } ] 
            ]
        ]
        |> fromTree



-- read


signature = Tree.Zipper.label >> .signature
item = Tree.Zipper.label >> .item


       
-- map


target : Signature -> Compositron -> Compositron
target sig =
    findFromRoot ( \node -> .signature node == sig )
    >> Maybe.withDefault trivial

untarget : Signature -> Compositron -> Compositron
untarget sig =
    root

choose : Signature -> Item -> Compositron -> Compositron
choose new choice =
    logTree
    >> Tree.Zipper.replaceTree ( Tree.tree { signature = new, item = choice }
                 [
                  Tree.singleton { signature = new++"inside", item = Ambiguous }
                 ] ) >> logTree
        >> addNode { signature = new++"before", item = Ambiguous } >> logTree

logTree = Debug.log "tree"
    
addNode : Node -> Compositron -> Compositron
addNode n z =
    case Tree.Zipper.parent z of
        Nothing -> z
        Just p -> p |>
           Tree.Zipper.mapTree ( \t -> Tree.mapChildren ( \c-> ( Tree.singleton n ) :: c ) t ) 
                
clear : Item -> Compositron -> Compositron
clear choice =
    mapLabel ( \node -> { node | item = Ambiguous } ) 


    
-- preview

preview :
    Signature
        -> ( Intent Compositron -> msg )
        -> Compositron
        -> Html msg
           
preview sig intent_to_message compositron =
    let
        click =
            create_intent sig compositron
            >> intent_to_message
            >> onClickNoBubble

        navigate_back =
            Untarget ( signature compositron )
            |> click

        header =
            h2 [] [ text sig ]
            
        ok =
            Html.label
                [ class "ok" ]
                [ button [ navigate_back ] [ text "OK" ] ]

        composition =
            compositron
                |> mapLabel ( \node -> { node | item = Highlight node.item } )
                |> both ( root >> tree, identity )
                |> toHtml sig intent_to_message

    in section [ class "compositron" ] [ header, composition, ok ]

       
toHtml :
    Signature
        -> ( Intent Compositron -> msg )
        -> ( Compositree, Compositron )
        -> Html msg
toHtml sig intent_to_message structure =
    let
        ( compositree, compositron ) =
            structure
                
        node =
            Tree.label compositree

        inner = \_->
            Tree.children compositree
            |> List.map (\child -> toHtml sig intent_to_message ( child, compositron ) )

        click =
            create_intent sig compositron
            >> intent_to_message
            >> onClickNoBubble

        navigate_here =
            Target node.signature
            |> click
               
        intend_choice option =
            Choose option
            |> click
              
    in  case node.item
        of
        Style ->
            div [ class "style", navigate_here ]
                <|  [ label [] [ text node.signature
                               , text ": style" ]
                    ] ++ ( inner () )

        Value string ->
            div [ class "value", navigate_here ]
                <| [ label [] [ text node.signature
                              , text ": value: " ]
                   , input [ class "input", value ( string |> Maybe.withDefault "" ) ][]
                   ] ++ ( inner () )
                
        Parag ->
            p   [ class "paragraph", navigate_here ]
                <| [ label [] [ text node.signature
                              , text ": paragraph" ]
                   ] ++ ( inner () )

        Ambiguous ->
            div [ class "ambiguous", navigate_here ]
                [ span [ class "ellipsis" ] [ text "..." ], label [] [ text node.signature ]
                ]
                
        Highlight i ->
           case i
            of
            Style ->
                div [ class "style targeted", navigate_here ]
                    <| [ label [] [ text node.signature
                                  , text ": style" ]
                       ] ++ ( inner () )
                    
            Value string ->
                div [ class "value targeted", navigate_here ]
                    <| [ label [] [ text node.signature
                                  , text ": value: " ]
                       , input [ class "input", value ( string |> Maybe.withDefault "" ) ][]
                       ] ++ ( inner () )
                    
            Parag ->
                  p   [ class "paragraph targeted", navigate_here ]
                      <| [ label [] [ text node.signature
                                    , text ": paragraph" ]
                         ] ++ ( inner () )
                      
            Ambiguous ->
                let
                    style =
                        button [ intend_choice Style ] [ text "Style" ]

                    value =
                        button [ intend_choice ( Value Nothing ) ] [ text "Value" ]

                    parag =
                        button [ intend_choice Parag ] [ text "Paragraph" ]
                 in
                  div [ class "ambiguous targeted", navigate_here ]
                      <| [ Html.label [] [ text node.signature ]
                         , style, value, parag
                         ]
                
            _ -> text ";"
                                                        



                 
-- edits

allNodes = tree >> Tree.flatten
           
allTargets = allNodes >> List.map ( \node -> Target node.signature )
           
allEdits = allTargets
    
type Edit
    = Target String
    | Untarget String
    | Choose Item
    | Clear Item


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
                Value _ -> "V"
                Parag -> "P"
                Style -> "S"
                _ -> "weird"
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


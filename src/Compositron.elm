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
    mapLabel ( \node -> { node | item = Value string } )
    
choose : Signature -> Item -> Map
choose new choice =
    logTree
    >> Tree.Zipper.replaceTree ( Tree.tree { signature = new, item = choice }
                 [
                  Tree.singleton { signature = new++"inside", item = Ambiguous }
                 ] ) >> logTree
        >> addNode { signature = new++"before", item = Ambiguous } >> logTree

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
                [ button [ navigate_back, accesskey 'x' ] [ text "OK" ] ]

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
              
    in  case node.item
        of
        Style ->
            div [ class "item style", navigate_here ]
                <|  [ label [] [ text node.signature
                               , text ": style" ]
                    ] ++ ( inner () )

        Value string ->
            let input_id = node.signature ++ "input"
            in label 
                   [ class "item value", navigate_here, for input_id ]
                <| [ label [ for input_id ] [ text node.signature , text ": value: " ]
                   , input [ id input_id
                           , class "input"
                           , value ( string |> Maybe.withDefault "" )
                           , set_this string
                           ][]
                   ] ++ ( inner () )
                
        Parag ->
            div [ class "item paragraph", navigate_here ]
                <| [ label [] [ text node.signature
                              , text ": paragraph" ]
                   ] ++ ( inner () )

        Ambiguous ->
            div [ class "item ambiguous", navigate_here ]
                [ button [ class "ellipsis" ] [ text "..." ], label [] [ text node.signature ]
                ]
                
        Highlight i ->
           case i
            of
            Style ->
                div [ class "item style targeted", navigate_here ]
                    <| [ label [] [ text node.signature
                                  , text ": style" ]
                       ] ++ ( inner () )
                    
            Value string ->
                let input_id = node.signature ++ "input"
                in label 
                       [ class "item value targeted", navigate_here, for input_id ]
                    <| [ label [ for input_id ] [ text node.signature
                                  , text ": value: " ]
                       , input [ id input_id
                               , name input_id
                               , class "input" 
                               , value ( string |> Maybe.withDefault "" ) 
                               , set_this string
                               ][]
                       ] ++ ( inner () )
                    
            Parag ->
                  p   [ class "item paragraph targeted", navigate_here ]
                      <| [ label [] [ text node.signature
                                    , text ": paragraph" ]
                         ] ++ ( inner () )
                      
            Ambiguous ->
                let
                    style =
                        button [ class "choice", choose_this Style ]
                               [ text "Style" ]

                    value =
                        button [ class "choice", choose_this ( Value Nothing ) ]
                               [ text "Value" ]

                    parag =
                        button [ class "choice", choose_this Parag ]
                               [ text "Paragraph" ]
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
                Value _ -> "V"
                Parag -> "P"
                Style -> "S"
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


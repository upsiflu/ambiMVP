module Compositron exposing
    ( State, trivial, preview )

import Tree exposing ( Tree )
import Tree.Zipper exposing  ( Zipper, mapLabel, root, tree, forward, append, findFromRoot, fromTree )
import Tuple exposing ( first, second )

import Html exposing ( Html )
import Html.Attributes exposing  ( .. )
import Html.Events exposing ( .. )
import Html.Lazy exposing ( .. )
import Json.Decode as Json
import Debug
import Browser.Dom as Dom
import Task
import Set exposing ( Set )

import Compositron.View as View exposing ( View )
import Compositron.Item as Item exposing ( Item )

import History.Intent exposing ( Intent )

import Helpers exposing ( .. )



-- Compositron

type alias State = Compositron

type alias Node = { signature : Item.Signature, item : Item }

type alias Compositron = Zipper Node -- has focus (hole)
type alias Compositree = Tree Node   -- same, but has no focus
 


{-- previous   
type Item

    kinds:
    = Layout {  Set Style )
    | Span Text Text
    | Parag
    
    automatically inserted:
    | Ambiguous
      
    only for producing Html:
    | Highlight Item
--}

                   
-- create


trivial : Compositron
trivial =
    Tree.singleton { signature = "start", item = Item.sectioning }
        |> fromTree {--
        |> (\_-> alternative )



alternative =
    let
        ambi sig fu =
            Tree.singleton { signature = sig, item = fu }
        span t = Item.span ( Just t )
    in Tree.tree { signature = "0", item = Item.paragraph }
        [ ambi "0.0"
        , Tree.tree { signature = "0.1", item = span "ABC" }
            [ ambi "0.1.0" ]
        , Tree.tree { signature = "0.2", item = Layout ( Set.singleton "red" ) }
            [ ambi "0.2.0"
            , Tree.tree { signature = "0.2.1", item = span "DEF" }
                [ ambi "0.2.1.0" ]  
            , Tree.tree { signature = "0.2.2", item = span "GHI" } 
                [ ambi "0.2.2.0" ] 
            ]
        , Tree.tree { signature = "0.3", item = Layout ( Set.singleton "shaded" ) }
            [ ambi "0.3.0"
            , Tree.tree { signature = "0.3.1", item = span "JKL" }
                [ ambi "0.3.1.0" ] 
            ]
        , Tree.tree { signature = "0.4", item = Layout ( Set.singleton "right" ) }
            [ ambi "0.4.0" ]
        ]
        |> fromTree

--}

-- read


signature = Tree.Zipper.label >> .signature
item = Tree.Zipper.label >> .item


       
-- map


type alias Map = Compositron -> Compositron
map_item fu = mapLabel ( \node -> { node | item = fu item } )

    
target : ( Item.Signature, Item.Signature ) -> Map
target ( sig, old ) =
    findFromRoot ( \node -> .signature node == sig )
    >> Maybe.withDefault trivial

        
modify : ( Maybe String, Maybe String ) -> Map
modify ( new, old ) =
    map_item <| \s -> case s of
        Item.T liquid frozen -> Item.T new frozen
        Item.Y _ -> Item.Y new
        Item.V _ -> Item.V new
        Item.U _ -> Item.U new
        x -> x

freeze : ( Maybe String, Maybe String ) -> Map
freeze ( new, old ) =
    map_item <| \s -> case s of
        Item.T ( Item.Text liquid frozen ) -> Item.T ( Item.Text liquid new )
        x -> x
    
choose : Item.Signature -> ( Item, Item ) -> Map
choose new ( cho, old ) =
    -- this is slightly hairy. We have to find out whether the ambi wants us to assumeSelf.
    let 
        logTree =
            Debug.log "tree"
        addNode n z =
            case Tree.Zipper.parent z of
                Nothing -> z
                Just p ->
                    p
                    |> Tree.Zipper.mapTree
                         ( \t -> Tree.mapChildren ( \c-> ( Tree.singleton n ) :: c ) t ) 
    in logTree
    >> if ( cho == Parag )
       then Tree.Zipper.replaceTree 
           ( Tree.tree 
                 { signature = new, item = cho }
                 [ Tree.singleton { signature = new++"inside", item = Ambiguous } ] 
           )
       else identity
    >> addNode 
            { signature = new++"before", item = Ambiguous }     
    >> logTree




-- edits

    
type Edit
    -- each Edit has ( New, Old )
    = Target ( Item.Signature, Item.Signature )
    | Choose ( Item, Item )
    | Modify ( Maybe String, Maybe String )
    | Freeze ( Maybe String, Maybe String )
    -- clear is simply the inverse of choose!

        
create_intent :
    Item.Signature
        -> Compositron
        -> Edit
        -> Intent Compositron
create_intent new_signature compositron edit =
    case edit of
        Target ( sig, old ) ->
            { serial = [ "🞋", new, old] |> String.join (" ")
            , function = target ( new, old )
            , inverse = target ( old, new )
            }
        Choose ( itm, old ) ->
            { serial = [ "!", Item.verbalize itm ] |> String.join (" ")
            , function = choose new_signature ( itm, old )
            , inverse = choose new_signature ( old, itm )
            }
        Modify ( str, old ) ->
            { serial = [ "~", enstring new, enstring old ] |> String.join (" ")
            , function = modify ( str, old )
            , inverse = modify ( old, str )
            }
        Freeze ( str, old ) ->
            { serial = [ "=", enstring new, enstring old ] |> String.join (" ")
            , function = freeze ( str, old )
            , inverse = freeze ( old, str )
            }


    
-- preview

type alias Message msg m =
    { m | from_intent : Intent Compositron -> msg
        , from_command : Cmd msg -> msg
        , noop : msg
    }

preview :
    Item.Signature
        -> Message msg m
        -> Compositron
        -> List ( Html msg )
           
preview sig message compositron =
    let
        incipit =
            h2 [ class "compositron", class "caption" ] [ text sig ]
          
        comp =
            compositron
                |> mapLabel ( \node -> { node | item = Highlight node.item } )
                |> both ( root >> tree, identity )
                |> toHtml sig message

    in incipit :: comp

destring string =
    case string of
        ""  -> Nothing
        str -> Just str
                      
enstring maybe =
    case maybe of
        Nothing  -> ""
        Just str -> str
       
toHtml :
    Item.Signature
        -> Message msg m
        -> ( Compositree, Compositron )
        -> List ( Html msg )
        
toHtml sig message structure =
    let
        ( compositree, compositron ) =
            structure
                
        node =
            Tree.label compositree

        inner = \_->
            Tree.children compositree
            |> List.map
               (\child -> toHtml sig message ( child, compositron ) )
            |> List.concat

        to_message =
            create_intent sig compositron
            >> message.from_intent

        navigate_here : Html.Attribute msg
        navigate_here =
            Target ( node.signature, sig )
            |> to_message >> onClickNoBubble

        focus_here : Html.Attribute msg
        focus_here =
            focus
            |> onClickNoBubble

        focus : msg
        focus =
            Dom.focus node.signature
            |> Task.attempt (\_->message.noop)
            |> message.from_command

               
        choose_this option =
            Choose option
            |> to_message >> onClickNoBubble

        input_url old =
            ( \new -> to_message ( Modify ( destring new, old ) ) )
            |> onInput

        input_span old =
            Json.map
                ( \new -> to_message ( Modify ( destring new, old ) ) )
                ( Json.at ["target", "textContent"] Json.string )
            |> on "input"

        blur_span =
            Freeze
            >> to_message >> onBlur
               
        view_item i =
            case i of
                
                Layout set ->
                    "Layout"
                        |> default_view 
                        |> View.icon ( text "S" )
                        |> View.item
                           ( View.element <| always span )
                        |> View.item
                           ( View.children <| (::) <|
                                 Html.input
                                 [ name ( node.signature ++ "-input" )
                                 , value ( verbalize_layout set )
                                 --, modify_this set
                                 ] []
                           )
                        
                Span string frozen_string ->
                    "Span"
                        |> default_view 
                        |> View.icon ( text "T" )
                        |> View.item
                           ( View.children <| (\t c -> c ++ t )
                                 [ span [ contenteditable False
                                        , classList [( "empty", frozen_string == Text Nothing )] ]
                                        [ frozen_string |> verbalize_text |> Html.text ]
                                 ]
                           )

                Parag ->
                    "Parag"
                        |> default_view 
                        |> View.icon ( text "P" )
                        |> View.item
                           ( View.element <| always Html.p )

                Ambiguous -> 
                    "Ambiguous"
                        |> default_view 
                        |> View.icon ( text "+" )
                        |> View.item
                           ( View.element ( always button )
                            >> View.attributes ( class "ellipsis" |> (::) ) 
                            >> View.children   ( [ Html.label [] [ Html.text "+" ] ] |> always )
                           )
                       
                _ ->
                    "Error"
                        |> default_view
             

        default_view =
            { focus_here = focus_here
            , navigate_here = navigate_here
            } |> View.default node.signature ( inner () )
                
                       
    in  case node.item
        of
            Highlight ( Span string frozen_string ) ->
                "Span"
                    |> default_view 
                    |> View.icon ( text "T" )
                    |> View.item
                       ( View.children <| (\t c -> c ++ t )
                             [ Html.span
                                   [ contenteditable True
                                   , input_span string
                                   , blur_span ( string, frozen_string )
                                   ]
                                   -- until newly-frozen, lazy Html won't rerender this node :-)
                                   [ frozen_string |> verbalize_text |> Html.text ]
                             ]
                       )
                    |> View.interactive
                    |> View.present

            Highlight ( Ambiguous ) ->
                let
                    choice_button cho =
                        Html.button
                            [ class "choice"
                            , choose_this ( cho, Highlight Ambiguous )
                            ]
                            [ verbalize_choice cho |> Html.text ] 
                in "Ambiguous"
                    |> default_view 
                    |> View.icon ( text "+" )
                    |> View.item
                       ( View.element ( always Html.span )
                         >> View.attributes ( class "ellipsis" |> (::) )
                         >> View.children
                           ( always <| List.map choice_button <|
                                 [ Layout Set.empty
                                 , Span ( Text Nothing ) ( Text Nothing )
                                 , Parag
                                 ]
                           )
                       )
                    |> View.interactive
                    |> View.present
             
            Highlight i -> view_item i |> View.interactive |> View.present

            i -> view_item i |> View.passive |> View.present
            
                                                        



                 

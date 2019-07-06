module Compositron exposing
    ( State, trivial, preview )

import Tree exposing ( Tree )
import Tree.Zipper exposing  ( Zipper, mapLabel, root, tree, forward, append, findFromRoot, fromTree )
import Tuple exposing ( first, second )

import Browser.Dom as Dom
import Debug
import Html exposing ( Html )
import Html.Attributes exposing  ( .. )
import Html.Events exposing ( .. )
import Html.Lazy exposing ( .. )
import Json.Decode as Json
import Set exposing ( Set )
import Task

import Compositron.Item as Item exposing ( Item )
import Compositron.View as View exposing ( View )

import History.Intent exposing ( Intent )

import Helpers exposing ( .. )



-- Compositron

type alias State = Compositron

type alias Node = { signature : Item.Signature, item : Item }

type alias Compositron = Zipper Node -- has focus (hole)
type alias Compositree = Tree Node   -- same, but has no focus
 

                   
-- create


trivial : Compositron
trivial =
    Tree.singleton { signature = "start", item = Item.Err "" }
        |> fromTree
        |> create_item ( always ( Item.Empty Item.paragraph ) )
        

-- read


signature = node >> .signature
item = node >> .item
node = Tree.Zipper.label
parent = \z -> z |> Tree.Zipper.parent |> Maybe.withDefault ( Tree.Zipper.root z )

         
-- map


type alias Map a = a -> a
    
map_item : Map Item -> Map Compositron
map_item fu =
    mapLabel <| \this-> { this | item = fu this.item }

create_item : Map Item -> Map Compositron
create_item fu z =
    let new_item = z |> item |> fu
        new_node = { signature = signature z, item = new_item }
        new_children = new_item |> Item.children |> List.indexedMap
                       ( \n child -> { signature = ( signature z ) ++ "/" ++ ( String.fromInt n )
                                     , item = child
                                     } |> Tree.singleton )
    in
        Tree.Zipper.replaceTree ( Tree.tree new_node new_children ) z

map_parent : Map Compositron -> Map Compositron
map_parent fu z =
    z   |> Tree.Zipper.parent
        |> Maybe.map fu
        |> Maybe.map ( Tree.Zipper.findNext ( (==) ( node z ) ) )
        |> Maybe.map ( Maybe.withDefault z )
        |> Maybe.withDefault z

map_children : Map Compositron -> Map Compositron
map_children fu z =
    z   |> Tree.Zipper.firstChild
        |> Maybe.map ( while_just ( fu >> Tree.Zipper.nextSibling ) )
        |> Maybe.map ( Tree.Zipper.parent )
        |> Maybe.map ( Maybe.withDefault z )
        |> Maybe.withDefault z

nonempty : Map Compositron
nonempty =
    let
        unfold_assumption z =
            case item z of
                Item.Assume fu -> List.foldl ( always >> create_item ) z ( fu True |> Item.form ) 
                other          -> z
        
    in \z->
        z |> case item z of
                 Item.Empty fill ->
                     map_item ( always fill )
                         >> map_children ( unfold_assumption )
                 full -> identity

            
target : ( Item.Signature, Item.Signature ) -> Map Compositron
target ( sig, old ) =
    findFromRoot ( \this -> this.signature == sig )
    >> Maybe.withDefault trivial

        
modify : ( Maybe String, Maybe String ) -> Map Compositron
modify ( new, old ) =
    map_item <| \s -> case s of
        Item.T ( Item.Text liquid frozen ) -> Item.T ( Item.Text new frozen )
        Item.Y _ -> Item.Y ( Item.Youtube new )
        Item.V _ -> Item.V ( Item.Vimeo new )
        Item.U _ -> Item.U ( Item.Url new )
        x -> x

freeze : ( Maybe String, Maybe String ) -> Map Compositron
freeze ( new, old ) =
    map_item <| \s -> case s of
        Item.T ( Item.Text liquid frozen ) -> Item.T ( Item.Text liquid new )
        x -> x
    
choose : Item.Signature -> ( Item, Item ) -> Map Compositron
choose new_signature ( itm, old ) =
        case Debug.log "chose item" itm of -- then we see what item was added
            Item.Ambiguous _ _ ->
                create_item ( always itm )
            any ->
                map_parent nonempty
                    >> create_item ( always ( Item.Empty any ) )




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
            { serial = [ "🞋", sig, old] |> String.join (" ")
            , function = target ( sig, old )
            , inverse = target ( old, sig )
            }
        Choose ( itm, old ) ->
            { serial = [ "!", Item.verbalize itm ] |> String.join (" ")
            , function = choose new_signature ( itm, old )
            , inverse = choose new_signature ( old, itm )
            }
        Modify ( str, old ) ->
            { serial = [ "~", enstring str, enstring old ] |> String.join (" ")
            , function = modify ( str, old )
            , inverse = modify ( old, str )
            }
        Freeze ( str, old ) ->
            { serial = [ "=", enstring str, enstring old ] |> String.join (" ")
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
            Html.h2 [ class "compositron", class "caption" ] [ Html.text sig ]
          
        comp =
            compositron
                |> mapLabel ( \this -> { this | item = Item.Highlight this.item } )
                |> both ( root >> tree, identity )
                |> to_html sig message

    in [ incipit, comp ]

destring string =
    case string of
        ""  -> Nothing
        str -> Just str
                      
enstring maybe =
    case maybe of
        Nothing  -> ""
        Just str -> str
       
to_html :
    Item.Signature
        -> Message msg m
        -> ( Compositree, Compositron )
        -> ( Html msg )
        
to_html sig message ( compositree, compositron ) =
    let
        this = Tree.label compositree

        -- elements
                
        inner = \_->
            Tree.children compositree
            |> List.map
               (\child -> to_html sig message ( child, compositron ) )
            

        -- interactivity

        to_message =
            create_intent sig compositron
            >> message.from_intent
                
        navigate_here : Html.Attribute msg
        navigate_here =
            Target ( this.signature, sig )
            |> to_message >> onClickNoBubble

        focus_here : Html.Attribute msg
        focus_here =
            Dom.focus this.signature
            |> Task.attempt (\_->message.noop)
            |> message.from_command
            |> onClickNoBubble
               
        choose_this : Item -> Html.Attribute msg
        choose_this option =
            Choose ( option, this.item )
            |> to_message >> onClickNoBubble

        input_url : Maybe String -> Html.Attribute msg
        input_url old =
            ( \new -> to_message ( Modify ( destring new, old ) ) )
            |> onInput

        input_span : Maybe String -> Html.Attribute msg
        input_span old =
            Json.map
                ( \new -> to_message ( Modify ( destring new, old ) ) )
                ( Json.at ["target", "textContent"] Json.string )
            |> on "input"

        blur_span : ( Maybe String, Maybe String ) -> Html.Attribute msg
        blur_span =
            Freeze
            >> to_message >> onBlur

        passive_or_interactive =
            case this.item of
                Item.Highlight a ->
                    View.interactive focus_here
                _ -> 
                    View.passive navigate_here   
    in
        Item.view
            { signature = this.signature
            , item = this.item
            }
            { choose_this = choose_this
            , input_url = input_url
            , input_span = input_span
            , blur_span = blur_span
            }
            { inner = inner
            }
            |> passive_or_interactive
            |> View.present

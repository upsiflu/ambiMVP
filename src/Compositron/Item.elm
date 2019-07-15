module Compositron.Item exposing  (..)

import Html exposing ( Html )
import Html.Attributes as Attributes

import Helpers exposing (..)

import Compositron.Data as Data exposing ( Data )
import Compositron.Signature as Signature exposing ( Signature )
import Compositron.View as View exposing ( View, Action (..) )



-- Item


type alias Name = String
type alias Frozen = Data
type alias Fluid = Data

type Assumption = Self
        
type Item
            
    -- elements
    = Assume Assumption -- when nonempty -- <

    | Field ( Name ) ( Flow ) ( List Item ) -- a named element -- body
    | Ambiguous ( Name ) ( () -> List Item ) -- same as field, but may be undecided -- amb. body

    | Info ( String ) -- an immutable label -- new
    | Err String
      
    -- editor for data (from a string) with variable width (span[contenteditable])
    | T Text
    | I Image
    | U Url
    | Y Youtube
    | V Vimeo

form : Item -> List Item
form itm =
    case itm of
        Field _ _ list -> list
        _-> []
    
verbalize : Item -> String
verbalize i =
    case i of
        Field n _ _ -> n
        Ambiguous n _ -> "Ambiguous "++n
        _ -> "?"

type Flow
    = Meta -- can be above, below, introducing, or trailing.
    | Inside
    | Stacked
    | Sectioning
    | Heading
    | Paragraph
    | Figure
    | Figcaption
    | Inaccessible -- for bodies that are not (yet) realized.
      
type Text
    -- while you edit text via a contenteditable span, the virtual DOM retains the frozen string\
    -- to keep the state within the Browser.
    = Text Fluid Frozen
      
type Image
    = Image Data

type Url
    = Url Data
      
type Youtube
    = Youtube Data
      
type Vimeo
    = Vimeo Data


      
-- read


-- emptiness
is_empty : ( () -> Bool ) -> Item -> Bool
is_empty cascade i =
    case i of
        Assume _ -> True
        Field _ _ _ -> cascade () -- this is a rubric, so its emptiness is decided further down.
        Ambiguous _ _ -> True -- TODO: here we assume that all ambiguities, even partial, are empty!

        Info _ -> True
        Err _ -> True
      
        concrete -> data concrete |> Data.is_empty

-- is type
is_assume_self i =
    case i of
        Assume Self -> True
        _ -> False

-- elements
data i =
    case i of
        T ( Text fluid frozen ) -> fluid
        I ( Image dat ) -> dat
        U ( Url dat ) -> dat
        Y ( Youtube dat ) -> dat
        V ( Vimeo dat ) -> dat
        _ -> Nothing
children i =
    case i of
        Field name flow kids -> kids
        _ -> []


             
-- set


set_data dat i =
    case i of
        T ( Text fluid frozen ) -> T ( Text dat frozen )
        Y _ -> Y ( Youtube dat )
        V _ -> V ( Vimeo dat )
        U _ -> U ( Url dat )
        x -> x

freeze i =
    case i of
        T ( Text fluid frozen ) -> T ( Text fluid fluid )
        x -> x

unfreeze i =
    case i of
        T ( Text fluid frozen ) -> T ( Text frozen frozen )
        x -> x
                                   

             
-- create (templates, or what would be an ambilang program)


layout =
    let style s = Field s Stacked [ Info s ]
    in
        Field "layout" Meta
            [ Assume Self
            , Ambiguous "+" <|\_->
                [ style "align right"
                , style "align centered"
                , style "shaded green"
                ]
            ]
            
blocks =
    Ambiguous "block..." <|\_->
        [ paragraph
        , heading
        , figure
        ]
        
spans =
    Ambiguous "in line..." <|\_->
        [ text Nothing
        , link Nothing Nothing
        ]

paragraph =
    Field "paragraph" Paragraph
        [ Assume Self
        , Ambiguous "+" <|\_->
            [ blocks
            , spans
            ]
        , layout
        ]

heading =
    Field "heading" Heading
        [ Assume Self
        , spans
        , layout
        ]

figure =
    Field "figure" Figure
        [ Ambiguous "figure media" <| \_->
            [ I <| Image ( Nothing )
            , Y <| Youtube ( Nothing )
            , V <| Vimeo ( Nothing )
            , blocks
            ]
        , Field "caption" Figcaption
            [ Ambiguous "caption" <| \_->
                  [ blocks
                  , spans
                  ]
            ]
        , layout
        ]

text t =
    Field "text" Inside
        [ Assume Self
        , spans
        , T <| Text t t
        , layout
        ]
        
link t u =
    Field "link" Inside
        [ Field "destination" Meta
            [ U <| Url u ]
        , text t
        , layout
        ]
        
proxy itm =
    Field "proxy" Stacked
        [ itm ]
        

            
-- present


-- functions not to expose
add_view =
    { proxy =
          view
          
    , flow = \f->
          let
              insert_class =
                  View.add_class <|
                      case f of
                          Meta -> "meta"
                          Inaccessible -> "inaccessible"
                          Stacked -> "stacked"
                          _ -> ""
              set_element =
                  View.element <| always <|
                      case f of
                          Inside -> Html.span -- default
                          Paragraph -> Html.p
                          Sectioning -> Html.section
                          Heading -> Html.h1
                          Figure -> Html.figure
                          Figcaption -> Html.figcaption
                          _ -> Html.button
          in insert_class >> set_element
              
    , info = \itm->
            case itm of
                Assume _ -> Info "<"
                Field name _ _ -> Info name
                Ambiguous name _ -> Info ( name ++ "..." )
                Info name -> Info name
                Err string -> Info string
                other -> proxy other
                
    , option = \n itm ->
          View.basic ( String.fromInt n ) ( Signature.irrelevant n ) ( always [] )
              |> view ( add_view.info itm )
              |> View.add_action ( Choose_this itm )
              |> View.element ( always Html.button )
    }

view : Item -> Map ( View msg Item Signature Data )
view itm =
    case itm of

        Assume _ ->
            View.add_class "Assume"
                      
        Field name flow items ->
            View.add_class name
                >> View.add_class "Field"
                >> add_view.flow flow

        Ambiguous name create_items ->
            View.add_class name
                >> View.add_class "Ambiguous"
                >> add_view.flow Meta
                >> View.set_text name
                >> View.children
                    ( (++)
                      ( create_items () |> List.indexedMap add_view.option )
                    )
 
        Info string ->
            View.add_class "info"
                >> View.set_text string

        T ( Text fluid frozen ) -> -- this is only the inner part, not the span around!
            View.add_class "input"
                >> View.add_class "T"

                >> View.add_action ( Targeted Contenteditable )
                >> View.add_action ( Targeted ( Input_span frozen ) )
                >> View.add_action ( Targeted Blur_span )

                -- view frozen span text to cope with contenteditable:
                >> case frozen of
                       Nothing -> identity
                       Just frozen_string ->
                           View.set_text frozen_string
                       
        I ( Image source ) ->
            View.add_class "image"
                >> View.add_class "I"
                >> View.element ( always Html.img )
                >> View.add_attribute ( Attributes.src ( Data.enstring source ) )

        U url ->
            View.add_class "url"
                >> View.add_class "U"
                >> View.element ( always Html.input )
                        
        Err string ->
            View.add_class string
                >> View.add_class "Err"
                
        _ -> 
            View.add_class "Todo"


serialize : Item -> String
serialize item =
    case item of
        Assume _ -> "<"
        Field n f l -> n
        Ambiguous n f -> "Ambiguous " ++ n
        Info s -> "🛈 " ++ s
        Err s -> "⚠ " ++ s
        T txt -> "Text"
        I image -> "Image"
        U irl -> "Url"
        Y youtube -> "Youtube"
        V vimeo -> "Vimeo"

          

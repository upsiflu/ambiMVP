module Compositron.Item exposing  (..)

import Compositron.View as View exposing ( View )

import Html exposing ( Html )
import Html.Attributes

-- item

{--
  Representation of an Item:

  Assume_Self snaps already on _choose_ because Choice is Data.
  Field creates an element over or under or in the flow.
  Ambiguous creates an item button that expands on targetting.
  By default, it is left from the flow so as to not interfere with other fields.
  Info is an Item 'inside' the flow that just displays some text.

  An Item is representative of the Ambilang that defines its behaviour.
  Insofar, Item = Body.
--}

type alias Name = String
type alias Data = Maybe String
type alias Frozen = Data
type alias Fluid = Data

type Assumption = Self
    
type alias Signature = String
-- Item signatures are derived from the Transformation signature of their instanciation.
    
type Item
            
    -- elements
    = Empty Item -- the only way to construct an Item. Its Group (all tree-children) is empty
    | Assume Assumption -- when nonempty -- <
    | Field ( Name ) ( Flow ) ( List Item ) -- a named element -- body
    | Ambiguous ( Name ) ( Bool -> List Item ) -- same as field, but may be undecided -- amb. body
    | Info ( String ) -- an immutable label -- new
      
    -- editor for data (from a string) with variable width (span[contenteditable])
    | T Text
    | I Image
    | U Url
    | Y Youtube
    | V Vimeo

    -- before mapping to a tree, the focus of the Zipper gets tagged:
    | Highlight Item -- only this item will show all controls and selectors.
    | Err String

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

is_empty i =
    case i of
        Empty _ -> True
        _ -> False
is_assume_self i =
    case i of
        Assume Self -> True
        _ -> False
             
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
        Empty ii -> children ii
        Field name flow kids -> List.map empty kids
        Highlight ii -> children ii
        _ -> []

-- map

empty : Item -> Item
empty itm = case itm of
    Field _ _ _ -> Empty itm
    _ -> itm
full : Item -> Item
full itm = case itm of
    Empty x -> x
    x -> x
      
-- macros

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
        


-- present


view node attributes elements =
   let
        extend this =
            view this attributes elements
                
        default_view =
            View.basic node.signature
            >> View.children elements.inner

        view_flow f =
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

        item_to_info i =
            case i of
                Empty ii -> item_to_info ii
                Assume _ -> Info "<"
                Field name _ _ -> Info name
                Ambiguous name _ -> Info ( name ++ "..." )
                Info name -> Info name
                Highlight ii -> item_to_info ii
                Err string -> Info string
                other -> other
                
        view_as_option n i =
            extend
                { signature = node.signature ++ "(" ++ ( String.fromInt n ) ++")" 
                , item = item_to_info i
                }
                |> View.attributes ( (::) ( attributes.choose_this i ) )
                |> View.element ( always Html.button )   
                |> View.preview
                
   in
       case node.item of

            Assume _ ->
                "<"
                    |> default_view
                    |> View.add_class "Assume"
                      
            Field name flow items ->
                name
                    |> default_view
                    |> View.add_class "Field"
                    |> view_flow flow

            Ambiguous name _ ->
                name
                    |> default_view
                    |> View.add_class "Ambiguous"
                    |> view_flow Meta
                    |> View.children ( (::) ( Html.text name ) )

            Highlight ( Ambiguous name create_items ) ->
                extend { node | item = Ambiguous name create_items }
                    |> View.children
                       ( List.indexedMap view_as_option ( create_items True ) |> (++) ) 

            Info string ->
                "info"
                    |> default_view
                    |> View.children ( (::) ( Html.text string ) )

            T ( Text fluid frozen ) -> -- this is only the inner part, not the span around it!
                "input"
                    |> default_view
                    |> View.add_class "T"
                    |> case frozen of
                        Nothing -> identity
                        Just frozen_string ->
                            [ frozen_string |> Html.text ] |> always |> View.children
                                
            Highlight ( T ( Text fluid frozen ) ) ->
                extend { node | item = T ( Text fluid frozen ) } 
                    |> View.attributes ( (::) ( Html.Attributes.contenteditable True ) )
                    |> View.attributes ( (::) ( attributes.input_span frozen ) )
                    |> View.attributes ( (::) ( attributes.blur_span ( fluid, frozen ) ) )
                                            
            I image ->
                let
                    source = case image of
                        Image Nothing -> ""
                        Image ( Just i ) -> i
                in
                    "image"
                        |> default_view
                        |> View.add_class "I"
                        |> View.element ( always Html.img )
                        |> View.attributes ( (::) ( Html.Attributes.src source ) )

            U url ->
                "url"
                    |> default_view
                    |> View.add_class "U"
                    |> View.element ( always Html.input )
                        
                       
            Highlight i -> extend { node | item = i }

            Empty i -> extend { node | item = i }
                    |> View.add_class "Empty"

            Err string -> string |> default_view
                    |> View.add_class "Err"
                
            _ -> "TODO" |> default_view
                    |> View.add_class "Todo"

module Compositron.Item exposing  (..)

import Compositron.View as View exposing ( View )

import Html exposing ( Html )

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
type alias Frozen_String = String
type alias Fluid_String = String

type alias Signature = String
-- Item signatures are derived from the Transformation signature of their instanciation.
    
type Item
            
    -- elements
    = Assume_self -- when nonempty
    | Field ( Flow ) ( Name ) ( List Item ) -- a named element
    | Ambiguous ( Name ) ( Bool -> List Item ) -- same as field, but may be undecided
    | Info ( String ) -- an immutable label
      
    -- editor for data (from a string) with variable width
    | T Text
    | I Image
    | U Url
    | Y Youtube
    | V Vimeo

    -- before mapping to a tree, the focus of the Zipper gets tagged:
    | Highlight Item -- this item will show all controls and selectors.
      

verbalize : Item -> String
verbalize i =
    case i of
        Field _ n _ -> n
        Ambiguous n _ -> "Ambiguous "++n
        _ -> "?"
      
type Flow
    = Above
    | Inside
    | Underneath
      
type Text
    -- while you edit text via a contenteditable span, the virtual DOM retains the frozen string\
    -- to keep the state within the Browser.
    = Text ( Maybe Fluid_String ) ( Maybe Frozen_String )
      
type Image
    = Image ( Maybe String )

type Url
    = Url ( Maybe String )
      
type Youtube
    = Youtube ( Maybe String )
      
type Vimeo
    = Vimeo ( Maybe String )


      
-- macros

layout =
    let style s = Field Above s [ Info s ]
    in
        Field Underneath "layout"
            [ Assume_self
            , Ambiguous "+" <| \_->
                [ style "align right"
                , style "align centered"
                , style "shaded green"
                ]
            ]
            
sectioning =
    Ambiguous "block..." <| \_->
        [ paragraph
        , heading
        , figure
        ]
        
phrasing =
    Ambiguous "in line..." <| \_->
        [ span Nothing
        , link Nothing
        ]

paragraph =
    Field Inside "paragraph"
        [ Assume_self
        , Ambiguous "+" <| \_->
            [ sectioning
            , phrasing
            ]
        , layout
        ]

heading =
    Field Inside "heading"
        [ Assume_self
        , phrasing
        , layout
        ]

figure =
    Field Inside "figure"
        [ Assume_self
        , Ambiguous "media" <| \_->
            [ I <| Image ( Nothing )
            , Y <| Youtube ( Nothing )
            , V <| Vimeo ( Nothing )
            , sectioning
            ]
        , Ambiguous "caption" <| \_->
            [ sectioning
            , phrasing
            ]
        , layout
        ]

span t =
    Field Inside "span"
        [ Assume_self
        , phrasing
        , T <| Text t t
        , layout
        ]
        
link u =
    Field Inside "link"
        [ Field Above "destination"
            [ U <| Url u ]
        , span ( Nothing )
        , layout
        ]


-- present


view : Item -> Html msg

view node attributes =
   let
        default_view =
            { focus_here = attributes.focus_here
            , navigate_here = attributes.navigate_here
            } |> View.default node.signature ( inner () )

        model =
            case node.item of
                
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
                               |> 
                                 Span string frozen_string ->
                                     "Span"
                                         |> default_view
                                         |> View.icon ( text "T" )
                                         |> View.item
                                            ( View.children <| (\t c -> c ++ t )
                                                  [ span
                                                        [ contenteditable False
                                                        , classList [( "empty", frozen_string == Text Nothing )] ]
                                                        [ frozen_string |> verbalize_text |> Html.text ]
                                                  ]
                                            )
                                         |>
                                           
                Highlight ( Span string frozen_string ) ->
                    "Span"
                        |> default_view
                        |> View.icon ( text "T" )
                        |> View.item
                           ( View.children <| (\t c -> c ++ t )
                                 [ Html.span
                                       [ contenteditable True
                                       , attributes.input_span string
                                       , attributes.blur_span ( string, frozen_string )
                                       ]
                                       -- until newly-frozen, lazy Html won't rerender this node :-)
                                       [ frozen_string |> verbalize_text |> Html.text ]
                                 ]
                           )
                        |> View.interactive
                        |> View.present
                                                                                                                                                                          
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
                        |> View.present_interactive
   
    in case node.item of
                       
    Highlight i -> view_item |> View.present_interactive

    i -> view_item |> View.present_passive
         
    _ ->
        "Error"
            |> default_view |> View.present_passive

module Compositron.View exposing
    ( View
    , Element
    , Action (..)

    -- create
    , basic

    -- map
    , element
    , children
    , activate
        
    -- add
    , add_id
    , add_action
    , add_class
    , add_attribute

    -- set
    , set_text

    -- view
    , present
    )


import Html exposing ( Html, Attribute )
import Html.Attributes as Attributes
import Html.Events exposing ( .. )
import Html.Lazy exposing ( .. )

import Helpers exposing ( .. )



type alias Element msg =
    List ( Attribute msg ) ->
    List ( Html msg ) ->
    Html msg

type Action item data
    = Navigate_here
    | Focus_here
    | Choose_this item
    | Input_url data
    | Input_span data
    | Blur_span
    | Contenteditable
    | Targeted ( Action item data )
    
type alias View msg item signature data =
    { descriptor : String
    , signature : signature
    , element : Element msg
    , text : String
    , actions : List ( Action item data )
    , attributes : List ( Attribute msg )
    , children : Children msg item signature data
    }

type Children msg item signature data =
    Children ( () -> List ( View msg item signature data ) )

force_children :
    View msg item signature data ->
    List ( View msg item signature data )
force_children v =
    case v.children of
        Children force_list -> force_list ()
        
actions fu =
    \v-> { v | actions = fu v.actions }
attributes fu =
    \v-> { v | attributes = fu v.attributes }
element fu =
    \v-> { v | element = fu v.element }
children :
    Map ( List ( View msg item signature data ) ) ->
    Map ( View msg item signature data )
children fu =
    \v-> { v | children =
               Children ( fu ( force_children v ) |> always ) }

add_action =
    (::) >> actions
add_attribute =
    (::) >> attributes
add_child =
    (::) >> children
         
add_class : String -> Map ( View msg item signature data )
add_class =
    Attributes.class >> add_attribute

add_id : String -> Map ( View msg item signature data )
add_id =
    Attributes.id >> add_attribute
        
set_text : String -> Map ( View msg item signature data )
set_text t =
    \v-> { v | text = t }


basic :
    String ->
    signature ->
    ( () -> List ( View msg item signature data ) ) ->
    View msg item signature data
basic descriptor sig inner =
    { descriptor = descriptor
    , signature = sig
    , element = Html.span
    , text = ""
    , actions = []
    , attributes = [ Attributes.class descriptor ]
    , children = Children inner
    }

activate : 
    ( signature -> ( Action item data -> Attribute msg ) ) ->
    Map ( View msg item signature data )
activate act v =
    v   |> attributes ( (++) ( List.map ( act v.signature ) v.actions ) )
        |> actions ( always [] )
        
preview :
    View msg item signature data ->
    Html msg
preview v =
    let
        html_children =
            ( Html.text v.text ) ::
            ( List.map preview ( force_children v ) ) 
    in lazy2
        v.element v.attributes html_children
            
    
present = preview

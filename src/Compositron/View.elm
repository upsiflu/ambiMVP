module Compositron.View exposing
    ( View
    , Element
    , Action (..)

    -- create
    , static
    , ephemeral
    , active

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
    , set_element
        
    -- view
    , present
    )


import Html exposing ( Html, Attribute )
import Html.Attributes as Attributes
import Html.Events exposing ( .. )
import Html.Lazy exposing ( .. )

import Helpers exposing ( .. )



type alias View msg sig cosig data =
    { descriptor : String
    , signature : Maybe sig
    , element : Element msg
    , text : String
    , actions : List ( Action cosig data )
    , attributes : List ( Attribute msg )
    , children : Children msg sig cosig data
    }

type alias Element msg =
    List ( Attribute msg ) ->
    List ( Html msg ) ->
    Html msg

type Action prototype data
    = Navigate_here
    | Focus_here
    | Choose_these ( List prototype )
    | Input_url data
    | Input_span data
    | Blur_span
    | Contenteditable
    | When_targeted ( Action prototype data )

type Children msg sig cosig data =
    Children ( () -> List ( View msg sig cosig data ) )

force_children :
    View msg sig cosig data ->
    List ( View msg sig cosig data )
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
    Map ( List ( View msg sig cosig data ) ) ->
    Map ( View msg sig cosig data )
children fu =
    \v-> { v | children =
               Children ( fu ( force_children v ) |> always ) }

add_action =
    (::) >> actions
add_attribute =
    (::) >> attributes
add_child =
    (::) >> children
         
add_class : String -> Map ( View msg sig cosig data )
add_class =
    Attributes.class >> add_attribute

add_id : String -> Map ( View msg sig cosig data )
add_id =
    Attributes.id >> add_attribute
        
set_text : String -> Map ( View msg sig cosig data )
set_text t =
    \v-> { v | text = t }

set_element : Element msg -> Map ( View msg sig cosig data )
set_element e =
    \v-> { v | element = e }
         
set_signature : Maybe sig -> Map ( View msg sig cosig data )
set_signature sig =
    \v-> { v | signature = sig }


static : String -> View msg sig cosig data
static descriptor =
    { descriptor = descriptor
    , signature = Nothing
    , element = Html.span
    , text = ""
    , actions = []
    , attributes = [ Attributes.class descriptor ]
    , children = Children ( always [] )
    }    

active :
    String ->
    sig ->
    ( () -> List ( View msg sig cosig data ) ) ->
    View msg sig cosig data
active descriptor sig inner =
    static descriptor
        |> children ( \_-> inner () )
        |> set_signature ( Just sig )

ephemeral :
    String ->
    sig ->
    View msg sig cosig data
ephemeral descriptor sig = active descriptor sig ( always [] )
    
activate : 
    ( sig -> ( Action prototype data -> Attribute msg ) ) ->
    Map ( View msg sig prototype data )
activate act v =
    case v.signature of
        Nothing ->
            v
        Just sig ->
            v   |> attributes ( (++) ( List.map ( act sig ) v.actions ) )
                |> clear_actions

clear_actions =
    actions ( always [] )
                   
preview :
    View msg sig cosig data ->
    Html msg
preview v =
    let
        html_children =
            ( Html.text v.text ) ::
            ( List.map preview ( force_children v ) ) 
    in lazy2
        v.element v.attributes html_children
            
    
present = preview

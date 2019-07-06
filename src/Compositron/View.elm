module Compositron.View exposing
    ( .. )


import Html exposing ( .. )
import Html.Attributes exposing  ( .. )
import Html.Events exposing ( .. )
import Html.Lazy exposing ( .. )

import Helpers exposing ( .. )


type alias Map msg = View msg -> View msg
type alias Element msg = List ( Attribute msg ) -> List ( Html msg ) -> Html msg

type alias View msg =
    { descriptor : String
    , element : Element msg
    , attributes : List ( Attribute msg )
    , children : Bool -> List ( Html msg )
    }

attributes fu =
    \v-> { v | attributes = fu v.attributes }
element fu =
    \v-> { v | element = fu v.element }
children fu =
    \v-> { v | children = \_-> fu ( v.children True ) }
         
interactive : Attribute msg -> Map msg
interactive focus_here =
    let focusable =
            focus_here |> (::) |> attributes
        targeted =
            class "targeted" |> (::) |> attributes
    in  targeted >> focusable                  
        
passive : Attribute msg -> Map msg
passive navigate_here =
    navigate_here |> (::) |> attributes

add_class : String -> Map msg
add_class c =
    c |> class |> (::) |> attributes
        
basic :
    String -> String -> View msg
 
basic signature descriptor =
    { descriptor = descriptor 
    , element = span
    , attributes =
          [ class descriptor
          , class "item"
          , id signature
          ]
    , children = always []
    }

preview : View msg -> Html msg
preview v =
    lazy2 v.element v.attributes ( v.children True )
    
present : View msg -> Html msg
present v =
    lazy2 v.element v.attributes ( v.children True )

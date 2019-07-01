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
    , icon : Html msg
    , focus_here : Attribute msg
    , navigate_here : Attribute msg
    , anchor : { element : Element msg
               , attributes : List ( Attribute msg )
               , children : List ( Html msg )
               }
    , item   : { element : Element msg
               , attributes : List ( Attribute msg )
               , children : List ( Html msg )
               }
    }

attributes fu =
    \x-> { x | attributes = fu x.attributes }
element fu =
    \x-> { x | element = fu x.element }
children fu =
    \x-> { x | children = fu x.children }
         
anchor fu =
    \v-> { v | anchor = fu v.anchor }
item fu =
    \v-> { v | item = fu v.item }
icon ico =
    \v-> { v | icon = ico }

present_interactive = interactive >> present
present_passive = passive >> present
         
interactive : Map msg
interactive v =
    let focusable =
            v.focus_here |> (::) |> attributes
        targeted =
            class "targeted" |> (::) |> attributes
    in  v |> ( anchor targeted ) >> ( item targeted ) >> ( item focusable )                  

passive : Map msg
passive v =
    let navigable =
            v.navigate_here |> (::) |> attributes
    in v |> item navigable

type alias Slot msg =
    { focus_here : Attribute msg
    , navigate_here : Attribute msg
    }
    
default :
    String ->
    List ( Html msg ) ->
    Slot msg ->
    String ->
    View msg
 
default signature inner slot descriptor =
    { descriptor = descriptor
    , icon = text "?"
    , focus_here = slot.focus_here
    , navigate_here = slot.navigate_here
    , anchor =
          { element = div
          , attributes =
                [ class descriptor
                , class "anchor"
                ]
          , children =
                [ label [ class "signature" ] [ text signature ] ]
          }
    , item =
          { element = span
          , attributes =
                [ class descriptor
                , class "item"
                , id signature
                , for ( signature ++ "-input" )
                ]
          , children = inner
          }
    }
        
present : View msg -> List ( Html msg )
present v =
    label [ class "item_icon" ] [ text v.descriptor ]
        |> before v.anchor.children
        |> lazy2 v.anchor.element v.anchor.attributes
        |> before [ lazy2 v.item.element v.item.attributes v.item.children ]

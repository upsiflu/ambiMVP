module Compositron.View exposing
    ( .. )


import Html exposing ( .. )
import Html.Attributes exposing  ( .. )
import Html.Events exposing ( .. )

import Helpers exposing ( .. )


type alias Map msg = View msg -> View msg
type alias Element msg = List ( Attribute msg ) -> List ( Html msg ) -> Html msg

    
type alias View msg =
    { descriptor : String
    , icon : Html msg
    , anchor : { element : Element msg
               , attributes : List ( Attribute msg )
               , children : List ( Html msg )
               }
    , item   : { element : Element msg
               , attributes : List ( Attribute msg )
               , children : List ( Html msg )
               }
    }

map_anchor : ( List ( Attribute msg ) -> List ( Attribute msg ) ) ->
             ( List ( Html msg ) -> List ( Html msg ) ) ->
             Map msg
map_anchor att_fu chi_fu =
    \v-> { v | anchor =
               { element    = v.anchor.element
               , attributes = att_fu v.anchor.attributes
               , children   = chi_fu v.anchor.children
               }
         }

map_item : Element msg ->
           ( List ( Attribute msg ) -> List ( Attribute msg ) ) ->
           ( List ( Html msg ) -> List ( Html msg ) ) ->
           Map msg
map_item ele att_fu chi_fu =
    \v-> { v | item =
               { element    = ele
               , attributes = att_fu v.item.attributes
               , children   = chi_fu v.item.children
               }
         }
    
set_icon : Html msg ->
           Map msg
set_icon ico =
    \v-> { v | icon = ico }
                 
interactive : Map msg
interactive =                                                                                        
    map_anchor                                                                               
        ( class "targeted" |> (::) )                                                             
        identity                                                                                  
    >> map_item button                                                                            
        ( (::) ( class "targeted" ) )                                                             
        identity

present : View msg -> List ( Html msg )
present v =
    label [ class "item_icon" ] [ text v.descriptor ]
        |> before v.anchor.children
        |> v.anchor.element v.anchor.attributes
        |> before [ v.item.element v.item.attributes v.item.children ]
    
default :
    String ->
    Attribute msg ->
    List ( Html msg ) ->
    String ->
    View msg

default signature navigate_here inner descriptor =
    { descriptor = descriptor
    , icon = text "?"
    , anchor =
          { element = button
          , attributes =
                [ class descriptor
                , class "anchor"
                , navigate_here
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
                ]
          , children = inner
          }
    }
        

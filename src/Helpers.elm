module Helpers exposing
    (..)

import Html.Events exposing ( .. )
import Json.Decode as Decode

-- types

type alias Map a = a -> a


-- maybe

perhaps : ( a -> Maybe a ) -> Map a
perhaps fu parameter =
    fu parameter
        |> Maybe.withDefault parameter

attempt : ( b -> Maybe a ) -> ( a -> ( b -> a ) )
attempt fu =
    \fallback -> fu >> Maybe.withDefault fallback
 
                 
-- conditional map

when : ( a -> Bool ) -> Map a -> Map a
when predicate fu probe =
    if predicate probe then fu probe else probe

        
-- functions in a map

apply : a -> ( a -> b ) -> b
apply parameter fu = fu parameter

                     
-- Tuple mapping
        
both ( f, g ) a = ( f a, g a )
all3 (f, g, h ) a = ( f a, g a, h a )
                  
each f ( a, b ) = ( f a, f b )
each3 f ( a, b, c ) = ( f a, f b, f c ) 

                      
-- Recursion

until : ( a -> Bool ) -> Map a -> Map a
until predicate succ variable =
    if predicate variable
    then variable
    else until predicate succ <| succ variable

while_just : Map a -> ( a  -> Maybe a ) -> Map a
while_just fu may_succ variable =
   if may_succ variable == Nothing
   then fu variable
   else may_succ ( fu variable )
       |> Maybe.withDefault ( fu variable )
       |> while_just fu may_succ

       
-- List

prepend : a -> Map ( List a )
prepend x xs = x::xs

before : List a -> a -> List a
before xs x = x::xs

-- Html helpers

onClickNoBubble message =
        Html.Events.custom
            "click"
            ( Decode.succeed
                 { message = message
                 , stopPropagation = True
                 , preventDefault = True
                 }
            )

last l =
    case l of 
              x::[] -> Just x
              x::xs -> last xs
              [] -> Nothing
              

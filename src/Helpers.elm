module Helpers exposing
    (..)

import Html.Events exposing ( .. )
import Json.Decode as Decode

-- maybe

perhaps : ( a -> Maybe a ) -> ( a -> a )
perhaps fu parameter =
    fu parameter
        |> Maybe.withDefault parameter

attempt : ( b -> Maybe a ) -> ( a -> ( b -> a ) )
attempt fu =
    \fallback -> fu >> Maybe.withDefault fallback
                 
-- conditional

when : ( b -> Bool ) -> ( a -> a ) -> ( b -> ( a -> a ) )
when predicate fu =
    \probe -> if predicate probe then fu else identity

-- functions in a map

apply : a -> ( a -> b ) -> b
apply parameter fu = fu parameter

-- Tuple mapping
        
both ( f, g ) a = ( f a, g a )
all3 (f, g, h ) a = ( f a, g a, h a )
                  
each f ( a, b ) = ( f a, f b )
each3 f ( a, b, c ) = ( f a, f b, f c ) 

-- Recursion

until : ( a -> Bool ) -> ( a -> a ) -> a -> a
until predicate succ variable =
    if predicate variable
    then variable
    else until predicate succ <| succ variable

while_just : ( a -> a ) -> ( a  -> Maybe a ) -> a -> a
while_just fu may_succ variable =
   if may_succ variable == Nothing
   then fu variable
   else may_succ variable |> Maybe.withDefault variable |> fu |> while_just fu may_succ

-- List

prepend : a -> List a -> List a
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
              

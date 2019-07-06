module Helpers exposing
    (..)

import Html.Events exposing ( .. )
import Json.Decode as Decode

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

        {-
unless : ( Maybe a -> Bool ) -> ( a -> Maybe a ) -> a -> a
unless predicate succ variable =
    let
        step p s recent v =
            if p v
            then recent
            else v |> Maybe.map s |> step p s v
    in step predicate succ ( Just variable ) ( Just variable )
        |> Maybe.withDefault ( variable )
         -}

while_just : ( a  -> Maybe a ) -> a -> a
while_just may_succ variable =
   if may_succ variable == Nothing
   then variable
   else may_succ variable |> Maybe.withDefault variable |> while_just may_succ
    
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
              

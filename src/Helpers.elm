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

-- List

prepend : a -> List a -> List a
prepend x xs = x::xs
        
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

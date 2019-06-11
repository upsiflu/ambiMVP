module Helpers exposing
    (..)

both ( f, g ) a = ( f a, g a )
all3 (f, g, h ) a = ( f a, g a, h a )
                  
each f ( a, b ) = ( f a, f b )
each3 f ( a, b, c ) = ( f a, f b, f c ) 
                  
until : ( a -> Bool ) -> ( a -> a ) -> a -> a
until predicate succ variable =
    if predicate variable
    then variable
    else until predicate succ <| succ variable
                                      

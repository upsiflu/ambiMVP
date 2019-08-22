module Helpers exposing
    (..)

import Html.Events exposing ( .. )
import Json.Decode as Decode


-- logging and debugging

trace text = \x -> Debug.log "▦" text |> always x
multitrace text = \x -> Debug.log "▤" text |> always x


-- aliases

type alias Map a = a -> a


-- wrappers

type Match a b
    = Skip
    | Pursue
    | Fix ( a -> b )

type Ambiguation a
    = Or a ( Ambiguation a )
    | Of a

ambiguation_to_list a =
    case a of
        Or x y -> x :: ( ambiguation_to_list y )
        Of x -> [x]


ambiguation_from_nonempty : Nonempty a -> Ambiguation a
ambiguation_from_nonempty n =
    case n of
        ( head, [] ) -> Of head
        ( head, [tail] ) -> Of tail |> Or head
        ( head, t::ail ) ->
            ambiguation_from_nonempty ( t, ail ) |> Or head
        

type alias Nonempty a =
    ( a, List a )

map_nonempty fu ( head, tail ) =
    ( fu head, List.map fu tail )

lzipper_singleton x = { before = [], focus = x, after = [] }
        
from_nonempty ( head, tail ) = head::tail

nonempty_to_lzipper ( head, tail ) = { before = [], focus = head, after = tail }

        
to_nonempty list =
    case list of
        l::ist -> Just ( l, ist )
        _ -> Nothing

to_lzipper list =
    case list of
        l::ist ->
            Just { before = [], focus = l, after = ist }
        _ ->
            Nothing
             
type alias LZipper a =
    { before : List a, focus: a, after: List a }

map_lzipper : ( a -> b ) -> LZipper a -> LZipper b
map_lzipper fu lza =
    { before = List.map fu lza.before
    , focus = fu lza.focus
    , after = List.map fu lza.after
    }

list_to_tuple : List a -> Maybe ( a, a )
list_to_tuple a =
    case a of
        [ x, y ] -> Just ( x, y )
        _ -> Nothing
        
-- maybe

must_cons : Maybe a -> Map ( Maybe ( List a ) )
must_cons may acc =
    case ( may, acc ) of
        ( Just x, Just xs ) ->
            Just ( x::xs )
        _ ->
            Nothing
        
may_cons : Maybe a -> Map ( List a )
may_cons may acc =
    case ( may, acc ) of
        ( Just x, xs ) ->
            x::xs
        ( Nothing, xs ) ->
            xs

filter_just : List ( Maybe a ) -> List a
filter_just =
    List.foldl may_cons []
                
fold_must : List ( Maybe a ) -> Maybe ( List a )
fold_must =
    List.foldl must_cons ( Just [] )

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
over = apply


-- flipping parameters

with base fu parameter =
    fu parameter base
                     
                     
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

after : List a -> a -> List a
after xs x = xs++[x]

keep_just : Maybe a -> Map ( List a )
keep_just may acc =
    case may of
        Just x -> x::acc
        _ -> acc
              


segment_at : ( a -> Bool ) -> List a -> List ( List a )
segment_at predicate =
     List.foldl
        ( \itm segments ->
              case ( segments, predicate itm ) of
                   ( _,         True ) -> [itm]::segments
                   ( seg::nts, False ) -> (itm::seg)::nts
                   ( [],       False ) -> []
        ) []
     >> List.map ( List.reverse ) >> List.reverse
    

              
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
              

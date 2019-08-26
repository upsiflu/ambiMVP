module Helpers exposing
    ( trace
    , multitrace

    , Map
    , Match (..)

    , map_if
              
    , Ambiguation (..)
    , ambiguation_to_list
    , ambiguation_from_nonempty
    , Nonempty
    , to_nonempty

    , over
    , with

    , both
    , all3
    , each
    , each3
        
    , perhaps
    , maybe
    , until
    , while_just
    , filter_just
    , fold_must
    , first_must
    , first_just

    , last
    , segment_at
    , list_to_tuple

    , onClickNoBubble
    )

{-| Convenience functions and types for [debugging](#logging-and-debugging), [nonempty](#Nonempty) and [ambiguous](#Ambiguation) wrappers, [matching two streams](#Match), [parameter fu](#parameter-shuffling), as well as convenience functions for [tuples](#tuple-convenience), [maybes](#fallability), [lists](#list-convenience) and [Html](#html-convenience).


# Logging and Debugging
@docs trace
@docs multitrace

# Type Aliases
@docs Map
@docs Nonempty
@docs to_nonempty

# Match
@docs Match

# Simple Conditionals
@docs map_if

# Ambiguation
@docs Ambiguation
@docs ambiguation_to_list
@docs ambiguation_from_nonempty

# Parameter Shuffling
@docs over
@docs with

# Tuple Convenience
@docs both
@docs all3
@docs each
@docs each3

# Fallability
@docs perhaps
@docs maybe
@docs while_just
@docs filter_just
@docs fold_must
@docs first_must
@docs first_just

# List Convenience
@docs segment_at
@docs list_to_tuple
@docs last
@docs until

# Html Convenience
@docs onClickNoBubble
-}

import Html.Events exposing ( .. )
import Json.Decode as Decode
import Helpers.LZipper exposing (..)
import Html as Html
import Maybe.Extra 

-- logging and debugging

{-| print a single line to the console.-}
trace : String -> Map b
trace text = \x -> Debug.log "-" text |> always x

{-| print multiple lines to the console.-}
multitrace : String -> Map b
multitrace text = \x -> Debug.log "~" text |> always x


-- aliases

{-| Endofunction over any type. -}
type alias Map a = ( a -> a )


-- wrappers

{-| The result of matching two structures 
that have the same shape but possibly different nodes.

    match x y z =
        if x == "skip" then 
            Skip
        else if x == y then 
            Keep
        else 
            Insert
-}
type Match
    = Skip
    | Keep
    | Insert

{-|Simple conditional.-}
map_if : Bool -> Map a -> Map a
map_if predicate fu =
    if predicate then fu else identity

      
{-| One or more possibilities. Isomorphic to [`Nonempty`](#Nonempty).-}
type Ambiguation a
    = Or a ( Ambiguation a )
    | Of a

{-| Lossy conversion to List.-}
ambiguation_to_list: Ambiguation a -> List a
ambiguation_to_list a =
    case a of
        Or x y -> x :: ( ambiguation_to_list y )
        Of x -> [x]

{-| Isomorphic conversion.-}
ambiguation_from_nonempty : Nonempty a -> Ambiguation a
ambiguation_from_nonempty n =
    case n of
        ( head, [] ) -> Of head
        ( head, [tail] ) -> Of tail |> Or head
        ( head, t::ail ) ->
            ambiguation_from_nonempty ( t, ail ) |> Or head
        
{-| A list with at least one element.-}
type alias Nonempty a =
    ( a, List a )

{-| Map a function over all members of nonempty.-}
map_nonempty : Map a -> Map ( Nonempty a )
map_nonempty fu ( head, tail ) =
    ( fu head, List.map fu tail )

{-| Lossy conversion to List.-}
from_nonempty : Nonempty a -> List a
from_nonempty ( head, tail ) = head::tail

{-| Fallable conversion from List.-}
to_nonempty : List a -> Maybe ( Nonempty a )
to_nonempty list =
    case list of
        l::ist -> Just ( l, ist )
        _ -> Nothing

{-| Fallable conversion from List.-}
list_to_tuple : List a -> Maybe ( a, a )
list_to_tuple a =
    case a of
        [ x, y ] -> Just ( x, y )
        _ -> Nothing
        
-- maybe

{-| Fallable rectification from a list. Use with List.foldl!-}
must_cons : Maybe a -> Map ( Maybe ( List a ) )
must_cons may acc =
    case ( may, acc ) of
        ( Just x, Just xs ) ->
            Just ( x::xs )
        _ ->
            Nothing

{-| Default a List of Maybes by removing Nothings. Use this filter with List.foldl!-}
may_cons : Maybe a -> Map ( List a )
may_cons may acc =
    case ( may, acc ) of
        ( Just x, xs ) ->
            x::xs
        ( Nothing, xs ) ->
            xs

{-| Keep just values, discard Nothings.-}
filter_just : List ( Maybe a ) -> List a
filter_just =
    List.foldl may_cons []

{-| If any element is Nothing, the whole List is Nothing.-}
fold_must : List ( Maybe a ) -> Maybe ( List a )
fold_must =
    List.foldl must_cons ( Just [] )

{-| If any element is Nothing, the whole List is Nothing.-}
first_must : List ( Maybe a ) -> Maybe a
first_must =
    List.foldl must_cons ( Just [] ) >> Maybe.andThen List.head

{-| The first `Just` result of _fu_ on elements of the list.-}
first_just : ( a -> Maybe b ) -> List a -> Maybe b
first_just fu list =
    case list of
        [] -> Nothing
        x::xs ->
           fu x |> Maybe.Extra.or ( first_just fu xs )
               
{-| Apply a Map only if it yields a just value, else apply identity.-}
perhaps : ( a -> Maybe a ) -> Map a
perhaps fu parameter =
    fu parameter
        |> Maybe.withDefault parameter

{-| Something weird is here.-}
attempt : ( b -> Maybe a ) -> ( a -> ( b -> a ) )
attempt fu =
    \fallback -> fu >> Maybe.withDefault fallback

{-| apply a Map only on a just parameter.-}
maybe : ( base -> Map a ) -> Maybe base -> Map a 
maybe fu may =
    case may of
        Just base -> fu base
        Nothing -> identity
                 
                 
-- conditional map

{-| Apply a Map to _a_ only if a predicate is true for _a_.-}
when : ( a -> Bool ) -> Map a -> Map a
when predicate fu probe =
    if predicate probe then fu probe else probe

        
-- functions in a map

{-| feed a function with a new parameter in pipe-forward style.
    
    ( (++) [ 1, 2, 3 ] )
        |> apply [ 4 ]
    --> [ 1, 2, 3, 4 ]
-}
apply : a -> ( a -> b ) -> b
apply parameter fu = fu parameter

{-| feed a function with a new parameter in pipe-forward style.
It is fobidden to mix `<|` and `|>`, i.e. forward pipes and backward pipes. 
`over` simulates `<|` in the forward context. 
    
    String.toUpper |> over "a" == "A"

    ( (++) [ 1, 2, 3 ] )
        |> over [ 4 ]
    --> [ 1, 2, 3, 4 ]
-}
over : a -> ( a -> b ) -> b
over = apply


-- flipping parameters

{-| add a new base behind a parameter of a function.-}
with : b -> ( p -> b -> r ) -> p -> r
with base fu parameter =
    fu parameter base
                     
                     
-- Tuple mapping

{-| diverge a parameter into a tuple by applying two different functions.-}
both : ( a -> f, a -> g ) -> a -> ( f, g )
both ( f, g ) a = ( f a, g a )

{-| diverge a parameter into a 3-tuple by applying three different functions.-}
all3 : ( a -> f, a -> g, a -> h ) -> a -> ( f, g, h )
all3 (f, g, h ) a = ( f a, g a, h a )

{-| map each element in a tuple with the same function.-}
each : ( a -> f ) -> ( a, a ) -> ( f, f )
each f ( a, b ) = ( f a, f b )

{-| map each element in a 3-tuple.-}
each3 : ( a -> f ) -> ( a, a, a ) -> ( f, f, f )
each3 f ( a, b, c ) = ( f a, f b, f c ) 

                      
-- Recursion

{-| Unless a _predicate_ is true, _iterate_ a _variable_..-}
until : ( a -> Bool ) -> Map a -> Map a
until predicate succ variable =
    if predicate variable
    then variable
    else until predicate succ <| succ variable

{-| ?-}
while_just : Map a -> ( a  -> Maybe a ) -> Map a
while_just fu may_succ variable =
   if may_succ variable == Nothing
   then fu variable
   else may_succ ( fu variable )
       |> Maybe.withDefault ( fu variable )
       |> while_just fu may_succ

       
-- List

{-| Fallably the last element in a List.-}
last : List a -> Maybe a
last l =
    case l of 
              x::[] -> Just x
              x::xs -> last xs
              [] -> Nothing              


{-| prepend a new item to an existing list. Same as `(::)`.-}
prepend : a -> Map ( List a )
prepend x xs = x::xs

{-| Flipped version of `(::)`.-}
before : List a -> a -> List a
before xs x = x::xs

{-| Flipped version of `(++)`.-}
after : List a -> a -> List a
after xs x = xs++[x]

{-| Use List.foldl for this filter.-}
keep_just : Maybe a -> Map ( List a )
keep_just may acc =
    case may of
        Just x -> x::acc
        _ -> acc
              

{-| Where _predicate_ holds, cut a list into sublists.-}
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

{-| Stop propagation as well as default operation on a click.-}
onClickNoBubble : msg -> Html.Attribute msg
onClickNoBubble message =
        Html.Events.custom
            "click"
            ( Decode.succeed
                 { message = message
                 , stopPropagation = True
                 , preventDefault = True
                 }
            )

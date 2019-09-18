module History exposing
    ( History
    , singleton -- wrapping a single State.
    , insert    -- inserting any Transformation on the same State type.
    , do        -- create Transformations for inserting later.
                -- They'll slot in right after the current future.
                -- TODO: You get a tuple with a unique, serialized signature.
    , browse_to -- walk (edges will be soft so you stay in bounds).
    , summary   -- past, present and future.
    , recent_signature_string -- recent signature
    )

{-|This structure lazily zips from an initial State over
transformations to an accumulated State, in both directions.
It can be fed with Transformations which will be inserted
in alphanumeric order.

# Motivation

History enables collaboration over unreliable networks:
Instances that receive the same Transformations in any order
eventually sync.

# Definition
@docs History

# Create
@docs singleton

# Append

Given a history, `do` can transform an Intent into a Transformation.

@docs do

# Map
@docs insert

# Read
@docs recent_signature_string
@docs summary

# Navigate
@docs browse_to
-}

import Helpers exposing ( .. )
import History.Transformation.Signature as Signature exposing ( Signature )
import History.Transformation as Transformation exposing ( Transformation, Copy ( .. ), compare )
import History.Intent as Intent exposing ( .. )

import List.Extra

{-|History is a zipper; it represents a hole in a sorted stack.
A Transformation can be inverted. History stores
positive transformations to the top of the current state
and inverted ones towards the bottom ("future"/"past").-}
type History s =
    History { future: List ( Transformation s )
            , present: s
            , past: List ( Inverse ( Transformation s ) )
            }
        
type Inverse x =
    Inverse x
        
        
-- create

{-|from a single state.-}
singleton: s -> History s
singleton s =
    History { past = [], present = s, future = [] }



        
{-| Tool to turn a Copy into a real Transformation by scheduling it for the future.-}
schedule : Transformation.Copy s -> History s -> Transformation s
schedule c =
    transformations >> last
        >> Maybe.withDefault ( Transformation.initial )
        >> Transformation.successive c




{-| Given an Intent, create a `Do` Copy, then slot on top of the given History.-}
do : Intent s -> History s -> Transformation s
do =
    Transformation.Do >> schedule

{-| Given a Signature, try to find the matching transformation in the past, then `Undo` and slot on top of the given History.-}
undo : Signature -> History s -> Transformation s
undo =
    Transformation.Undo >> schedule


   

            
-- read


{-| Used e.g. to create Creators in Compositron.-}
recent_signature_string : History s -> String
recent_signature_string =
    transformations
        >> last
        >> Maybe.withDefault ( Transformation.initial )
        >> Transformation.signature >> Signature.serialize

        
         
                   
-- map
        
{-| The transformation will be sorted according to `Transformation.isBelow`, `Transformation.isAbove`. The state will only be affected if the new transformation is _below_ the current transformation.-}
insert : Transformation s -> Map ( History s )
insert t h =
    let here =   map_future ( (::) ( {-Debug.log "new transformation arrived in history"-} t ) )
        lower =  prev >> insert t >> next
        higher = next >> insert t >> prev
    in
    case ( past h, future h ) of
        ( [],     [] ) ->
            here h
        ( ( Inverse p )::ast, [] ) ->
            if      Transformation.compare ( Signature.is_below ) p t then lower h else here h
        ( [],     f::uture ) ->
            if      Transformation.compare ( Signature.is_above ) f t then higher h else here h
        ( ( Inverse p )::ast, f::uture ) ->
            if      Transformation.compare ( Signature.is_below ) p t then lower h
            else if Transformation.compare ( Signature.is_above ) f t then higher h else here h


remove : ( Transformation s -> Bool ) -> Map ( History s )
remove fil h =
    let             
        new_past =
            past h |> List.filter ( uninverse >> fil >> not )
        old_future =
            future h
    in
        bottom h
            |> map_future ( \_-> new_past |> List.map uninverse )
            |> top
            |> map_future ( \_-> old_future )

            
                

-- browse

{-| Use Nothing to jump to the top, `Just <Int>` for absolute positions after zero.-}
browse_to : Maybe Int -> History s -> History s
browse_to to h =
    let position = \_-> past h |> List.length
    in h |> case to of
        Nothing ->
            top
        Just n ->
            browse ( Just ( n - position () ) )

{-| Use Nothing to jump to the top, `Just <Int>` to move relative to your current position.-}
browse : Maybe Int -> History s -> History s
browse by =
    case by of
        Nothing ->
            top
        Just 0 ->
            identity
        Just n ->
            if n > 0 then browse ( n-1 |> Just ) >> next
            else browse ( n+1 |> Just ) >> prev

prev : History s -> History s
prev h =
    case past h of
        [] -> h
        ( Inverse p )::ast ->
            case Transformation.copy p of
                Undone trans ->
                    --reinsert the previously undone transformation 
                    h |> map_past ( \_-> ast )
                      |> insert trans
                      |> map_future ( (::) p )
                Do intent ->
                    --unapply the intent
                    h |> map_past ( \_-> ast )
                      |> map_present intent.inverse
                      |> map_future ( (::) p )
                _ ->
                    h |> map_past ( \_-> ast )

next : History s -> History s
next h =
    case future h of
        [] -> h
        f::uture ->
            case Transformation.copy f of
                Undo undo_sig ->
                    h |> map_future ( \_-> uture )
                      |> case past h |> List.Extra.find ( uninverse >> Transformation.signature >> (==) undo_sig ) of
                             Nothing ->
                                 map_past ( (::) ( Inverse f ) )
                             Just ( Inverse to_undo ) ->
                                 remove ( Transformation.is undo_sig )
                                     >> map_past ( (::) ( Inverse ( f |> Transformation.undone to_undo ) ) )
                Do intent ->
                    h |> map_future ( \_-> uture )
                      |> map_present intent.function
                      |> map_past ( (::) ( Inverse f ) )
                _ ->
                    h |> map_future ( \_-> uture )


isTop : History s -> Bool                
isTop h = future h == []

top : History s -> History s
top = next |> until isTop

isBottom : History s -> Bool
isBottom h = past h == []

bottom : History s -> History s
bottom = prev |> until isBottom



-- for further consumption or debugging

{-| A grab bag of stuff.-}
summary :
    History s ->
    { past : List ( String, String )
    , present : s
    , future : List ( String, String )
    }
summary h = { past = past h |> List.reverse >> List.map ( uninverse >> Transformation.serialize )
            , present = present h
            , future = future h |> List.map Transformation.serialize
            }


-- type helpers

future ( History h ) = h.future
map_future f ( History h ) = History { h | future = f h.future }

past ( History h ) = h.past

map_past : Map ( List ( Inverse ( Transformation s ) ) ) -> Map ( History s )
map_past f ( History h ) = History { h | past = f h.past }


present ( History h ) = h.present
map_present f ( History h ) = History { h | present = f h.present }
                      
transformations : History s -> List ( Transformation s )
transformations =
    both ( past >> List.reverse >> List.map uninverse, future ) >> \(a, b) -> a++b



uninverse : Inverse i -> i
uninverse ( Inverse i ) = i
       

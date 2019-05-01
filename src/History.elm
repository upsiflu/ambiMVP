module History exposing
    ( History   -- only used in main. Can we get rid of this?
    , singleton -- wrapping a State.
    , insert    -- inserting any Transformation on the same State type.
    , do, undo  -- create Transformations for appending to the history. They will slot in right after the current future.
    , browse    -- walk around (edges will be soft).
    , summary   -- a useful summary of the current components.
    )
import Helpers exposing (..)
import List.Extra exposing ( last )
import Transformation exposing ( Transformation )



{-  HISTORY

    This structure lazily zips from an initial State over
    transformations to an accumulated State, in both directions.
    It can be fed with Transformations which will be inserted
    in alphanumeric order.

    History enables collaboration over unreliable networks:
    Instances that receive the same Transformations in any order
    eventually sync.

    History is a zipper; it represents a hole in a stack.
    A Transformation can be inverted. History stores
    positive transformations to the top of the current state
    and inverted ones towards the bottom ("future"/"past"). -}





type History s =
    History { past: List ( Transformation s )
            , state: s
            , future: List ( Transformation s )
            }

        
-- create and append
        
singleton : s -> History s
singleton s =
    History { past = [], state = s, future = [] }

-- what dows it do?
-- It takes a history and an action.
-- It wraps the action into a Copy.
-- It finds the latest transformation in the history and creates a successor.

slot : History s -> Transformation.Copy s -> Transformation s
slot = transformations >> last >> Maybe.withDefault ( Transformation.initial ) >> Transformation.successive
    
do : History s -> { function : s -> s, inverse : s -> s, serial : String } -> Transformation s
do h =
    Transformation.do >> ( slot h )

             
undo : History s -> Transformation s -> Transformation s
undo h =
    Transformation.undo >> ( slot h )

                   
-- map
        
insert : Transformation s -> History s -> History s
insert t h =
    let here =   mapFuture ( (::) t )
        lower =  prev >> insert t >> next
        higher = next >> insert t >> prev
    in
    case ( past h, future h ) of
        ( [],     [] ) ->
            here h
        ( p::ast, [] ) ->
            if      Transformation.isBelow p t then lower h else here h
        ( [],     f::uture ) ->
            if      Transformation.isAbove f t then higher h else here h
        ( p::ast, f::uture ) ->
            if      Transformation.isBelow p t then lower h
            else if Transformation.isAbove f t then higher h else here h



-- browse

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
        p::ast ->
            History { past = ast
                    , state = state h |> Transformation.engage p
                    , future = ( Transformation.invert p )::( future h ) }

next : History s -> History s
next h =
    case future h of
        [] -> h
        f::uture ->
            History { past = ( Transformation.invert f )::( past h )
                    , state = state h |> Transformation.engage f
                    , future = uture }




isTop h = future h == []

top : History s -> History s
top = next |> until isTop

isBottom h = past h == []

bottom : History s -> History s
bottom = prev |> until isBottom



-- for further consumption

summary h = { past = past h |> List.reverse >> List.map Transformation.serialize, state = state h, future = future h |> List.map Transformation.serialize }



-- type helpers

future ( History h ) = h.future
mapFuture f ( History h ) = History { h | future = f h.future }
                       
past ( History h ) = h.past

state ( History h ) = h.state

transformations : History s -> List ( Transformation s )
transformations =
    both ( past >> List.reverse, future ) >> \(a, b) -> a++b

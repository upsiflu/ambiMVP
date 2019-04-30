module History exposing
    ( History   -- only used in main. Can we get rid of this?
    , singleton -- wrapping a State.
    , insert    -- appending any Transformation on the same State type.
    , beyond    -- a Transformation that will go beyond the future of this History.
    , browse    -- walk around (edges will be soft).
    , summary   -- a useful summary of the current components.
    )
import Helpers exposing (..)
import List.Extra exposing ( last )
import Transformation exposing (..)


{-  HISTORY

    This structure lazily zips from an initial State over
    transformations to an accumulated State, in both directions.
    It can be fed with Transformations which will be inserted
    in alphanumeric order.

    The only way to create a Transformation is to demand
    an "additional" on an existing History, which will always
    reference the author of this History and the most recently
    received Transformation (the contextual). Thus, all Transfor-
    mation Signatures are unique as well as authored.

      --> Todo: Add authoring (later...)

    History enables collaboration over unreliable networks:
    Instances that receive the same Transformations in any order
    eventually synch.

    History is a zipper; it represents a hole in a stack.
    A Transformation can be inverted. History stores
    positive transformations to the top of the current state
    and inverted ones towards the bottom ("future"/"past"). -}





type History s =
    History { past: List ( Transformation s )
            , state: s
            , future: List ( Transformation s )
            }
    
-- create
        
singleton : s -> History s
singleton s =
    History { past = [], state = s, future = [] }

                                    

-- modify
        
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

beyond : History s -> ( Transformation.Copy s -> Transformation s )
beyond =
     transformations >> last >> Transformation.follow
    


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
            History { past = ast, state = state h |> engage p, future = ( invert p )::( future h ) }

next : History s -> History s
next h =
    case future h of
        [] -> h
        f::uture ->
            History { past = ( invert f )::( past h ), state = state h |> engage f, future = uture }




isTop h = future h == []

top : History s -> History s
top = next |> until isTop


isBottom h = past h == []

bottom : History s -> History s
bottom = prev |> until isBottom



-- for further consumption

summary h = { past = past h |> List.reverse >> List.map serialize, state = state h, future = future h |> List.map serialize }



-- type helpers

future ( History h ) = h.future
mapFuture f ( History h ) = History { h | future = f h.future }
                       
past ( History h ) = h.past

state ( History h ) = h.state

transformations : History s -> List ( Transformation s )
transformations =
    both ( past >> List.reverse, future ) >> \(a, b) -> a++b

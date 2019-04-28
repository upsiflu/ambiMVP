module History exposing
    ( History   -- only used in main. Can we get rid of this?
    , singleton -- wrapping a State.
    , insert    -- appending any Transformation on the same State type.
    , browse    -- walk around (edges will be soft).
    , state     -- the state you walked to.
    , own       -- your own transformation, ready to append to the future.
    , view )    -- representation

import Transformation exposing (..)
import Helpers exposing (..)



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


        
-- read

state : History s -> s
state ( History h ) = h.state



-- modify
        
insert : Transformation s -> History s -> History s
insert t ( History h ) =
    let here =   \_->
                 History { past = h.past, state = h.state, future = t::h.future } |> next |> prev
        lower =  \_->
                 History h |> prev >> insert t >> next
        higher = \_->
                 History h |> next >> insert t >> prev
    in case ( h.past, h.future ) of
        ( [],     [] ) ->
            here ()
        ( p::ast, [] ) ->
            if      Transformation.isBelow p t then lower () else here ()
        ( [],     f::uture ) ->
            if      Transformation.isAbove f t then higher () else here ()
        ( p::ast, f::uture ) ->
            if      Transformation.isBelow p t then lower ()
            else if Transformation.isAbove f t then higher () else here ()

                
own ( History h ) =
    let ( ini, con ) =
            h.future |> List.reverse |> \l->l++h.past
                     |> both ( List.reverse, identity )
                     |> each List.head
                     |> each ( Maybe.withDefault ( initial "flupsi" h.state ) )
    in follow ini con



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
prev ( History h ) =
    case h.past of
        [] ->
            History h
        p::ast ->
            History { past = ast, state = ( h.state |> engage p ), future = ( invert p )::h.future }

next : History s -> History s
next ( History h ) =
    case h.future of
        [] ->
            History h
        f::uture ->
            History { past = ( invert f )::h.past, state = ( h.state |> engage f ), future = uture }




isTop ( History h ) = h.future == []

top : History s -> History s
top = next |> until isTop


isBottom ( History h ) = h.past == []

bottom : History s -> History s
bottom = prev |> until isBottom



-- present

view ( History h ) = { past = List.map serialize ( List.reverse h.past ), future = List.map serialize h.future }




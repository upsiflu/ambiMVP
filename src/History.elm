module History exposing ( singleton, insert )
import Transformation exposing ( Transformation, engage, invert, isAbove, isBelow )


{-  History

    This structure caches the latest State of the application.
    It can be fed with Transformations which will be inserted
    in alphanumeric order.

    History enables collaboration over unreliable networks:
    Instances that receive the same Transformations in any order
    eventually synch.

    History is a zipper; it represents a hole in a stack.
    A Transformation can be inverted. History stores
    positive transformations to the top of the current state
    and inverted ones towards the bottom.

        singleton State -> a zipper without transformation.
        insert Transformation History -> pushes a transformation
          at the orderly position, rebuilding next states.
        current -> State at the current position in the stack.
        prev -> travel down the stack.
        next -> travel up the stack.
        top -> integreta all transformations in order.
        bottom -> suspend all transformations in order.

-}





type History s =
    History { past: List ( Transformation s )
            , state: s
            , future: List ( Transformation s )
            }


singleton : s -> History s
singleton s = History { past = [], state = s, future = [] }

insert : Transformation s -> History s -> History s
insert t ( History h ) =
    let here = History { past = t::h.past, state = ( h.state |> engage t ), future = h.future }
        lower = History h |> prev >> insert t >> next
        higher = History h |> next >> insert t >> prev
    in case ( h.past, h.future ) of
        ( [],     [] ) ->
            here
        ( p::ast, [] ) ->
            if      Transformation.isBelow p t then lower else here
        ( [],     f::uture ) ->
            if      Transformation.isAbove f t then higher else here
        ( p::ast, f::uture ) ->
            if      Transformation.isBelow p t then lower
            else if Transformation.isAbove f t then higher else here


prev : History s -> History s
prev ( History h ) =
    case h.past of
        [] ->
            History h
        p::ast ->
            History { past = ast, state = ( h.state |> engage p ), future = ( Transformation.invert p )::h.future }

next : History s -> History s
next ( History h ) =
    case h.future of
        [] ->
            History h
        f::uture ->
            History { past = ( Transformation.invert f )::h.past, state = ( h.state |> engage f ), future = uture }


current : History s -> s
current ( History h ) = h.state


isTop ( History h ) = h.future == []

top : History s -> History s
top = next |> until isTop


isBottom ( History h ) = h.past == []

bottom : History s -> History s
bottom = prev |> until isBottom








--------------- HELPERS ---------------

        
until : ( a -> Bool ) -> ( a -> a ) -> a -> a
until predicate succ variable =
    if predicate variable then variable else until predicate succ <| succ variable

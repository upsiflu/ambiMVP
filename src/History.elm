module History exposing ( History, singleton, insert, browse, state, transformation, view )
import Transformation exposing ( Transformation, engage, invert, isAbove, isBelow, serialize )


{-  History


        singleton State -> a zipper without transformation.
        insert Transformation History -> pushes a transformation
          at the orderly position, rebuilding next states.
        current -> State at the current position in the stack.
        prev -> travel down the stack.
        next -> travel up the stack.
        top -> integreta all transformations in order.
        bottom -> suspend all transformations in order.


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


-}





type History s =
    History { past: List ( Transformation s )
            , state: s
            , future: List ( Transformation s )
            }


singleton : s -> History s
singleton s = Debug.log "creating a singleton History" <|
    History { past = [], state = s, future = [] }

insert : Transformation s -> History s -> History s
insert t ( History h ) =
    let here =   \_-> History { past = ( invert t )::h.past, state = Debug.log "state" ( h.state |> engage t ), future = h.future }
        lower =  \_-> History h |> prev >> insert t >> next
        higher = \_-> History h |> next >> insert t >> prev
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

browse by =
    case Debug.log "browse by" by of
        Nothing -> top
        Just 0 -> identity
        Just n -> if n > 0 then browse ( n-1 |> Just ) >> next
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


state : History s -> s
state ( History h ) = h.state

transformation : History s -> Transformation s
transformation ( History h ) = h.past |> List.head |> Maybe.withDefault Transformation.trivial

                               
isTop ( History h ) = h.future == []

top : History s -> History s
top = next |> until isTop


isBottom ( History h ) = h.past == []

bottom : History s -> History s
bottom = prev |> until isBottom

view ( History h ) = { past = List.map serialize h.past, future = List.map serialize h.future }




--------------- HELPERS ---------------

        
until : ( a -> Bool ) -> ( a -> a ) -> a -> a
until predicate succ variable =
    if predicate variable then variable else until predicate succ <| succ variable

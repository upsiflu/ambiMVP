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
import Helpers exposing ( .. )
import History.Transformation as Transformation exposing ( Transformation, Copy ( .. ) )
import History.Intent as Intent exposing ( .. )


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
    History { past:    List ( Transformation s )
            , present: s
            , future:  List ( Transformation s )
            }

        
-- create and append
        
singleton:s -> History s
singleton s =
    History { past = [], present = s, future = [] }

slot : History s -> Transformation.Copy s -> Transformation s
slot =
    transformations
        >> last
        >> Maybe.withDefault ( Transformation.initial )
        >> Transformation.successive

recent_signature_string =
    transformations
        >> last
        >> Maybe.withDefault ( Transformation.initial )
        >> Transformation.serialize_signature

do : History s -> Intent s -> Transformation s
do h =
    Transformation.do >> ( slot h )

         
                   
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

browse_to : Maybe Int -> History s -> History s
browse_to to h =
    let position = past h |> List.length |> always
    in h |> case to of
        Nothing ->
            top
        Just n ->
            browse ( Just ( n - position () ) )

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
        p::ast ->
            History { past    = ast
                    , present = present h |> Transformation.engage p
                    , future  = ( Transformation.invert p )::( future h )
                    }

next : History s -> History s
next h =
    case future h of
        [] -> h
        f::uture ->
            History { past    = ( Transformation.invert f )::( past h )
                    , present = present h |> Transformation.engage f
                    , future  = uture
                    }


isTop : History s -> Bool                
isTop h = future h == []

top : History s -> History s
top = next |> until isTop

isBottom : History s -> Bool
isBottom h = past h == []

bottom : History s -> History s
bottom = prev |> until isBottom



-- for further consumption or debugging

summary h = { past = past h |> List.reverse >> List.map Transformation.serialize
            , present = present h
            , future = future h |> List.map Transformation.serialize
            }


-- type helpers

future ( History h ) = h.future
mapFuture f ( History h ) = History { h | future = f h.future }

past ( History h ) = h.past

present ( History h ) = h.present
mapPresent f ( History h ) = History { h | present = f h.present }
                      
transformations : History s -> List ( Transformation s )
transformations =
    both ( past >> List.reverse, future ) >> \(a, b) -> a++b

module Compositron.Structure.Branch exposing
    ( Branch

    -- create
    , singleton
    , create

    -- read
    , node
    , kids
    , is
    , first

    -- map (with a primer)
    , accept

    -- serial form
    , serialize
    , to_string
    , deserialize
    , from_string
    , log
    )

{-| Branch (Tree).

# Definition
@docs Branch

# Create
@docs singleton
@docs create

# Read
@docs node
@docs kids
@docs is
@docs first

# Map (with a primer)
@docs accept

# Serial form
@docs serialize
@docs to_string
@docs deserialize
@docs from_string
@docs log
-}

import Tree exposing ( Tree )

import Helpers exposing (..)


import Maybe.Extra
import String.Extra

{-| Tree of nodes _a_ -}
type alias Branch a =
    Tree a


-- create


{-| Create a Branch from a single node.

    singleton "a" |> to_string
    --> "a"
-}
singleton : a -> Branch a
singleton = Tree.singleton

{-| Create a Branch from a single node.

    create "a" [ singleton "b" ] |> to_string
    --> "a\n  b"
-}
create : a -> List ( Branch a ) -> Branch a
create = Tree.tree
            



-- read


{-| Node at the root of the given branch.

    singleton 1 |> node 
    --> 1
-}
node : Branch a -> a
node = Tree.label



{-| Subbranches (descendants) of the given branch.

    from_string "a\n  b\n  c"
        |> kids
        |> List.map node
    --> ["b", "c"]
-}
kids : Branch a -> List ( Branch a )
kids = Tree.children




-- modify (with a primer)


{-| Map a branch while counting with a primer.

    ( 0, singleton "a" )
        |> accept
             (\pre tmp -> ( pre+1, tmp ) )
    --> ( 1, singleton "a" )
-}
accept :
    ( p -> t -> ( p, a ) ) ->
    ( p, Branch t ) ->
    ( p, Branch a )
accept next ( primer, template_branch ) =
    Tree.mapAccumulate next primer template_branch






{-| Given a comparison function between their nodes,
compare two branches of different domains. Will return True only
if the structure is identical and all nodes compare favorably.

    is (==)
        ( from_string "a\n  2" )
        ( from_string "a\n  2" )
        --> True

    is (<)
        ( singleton 0 )
        ( singleton 1 )
        --> True
 -}
is :
    ( a -> t -> Bool ) -> Branch a -> Branch t -> Bool
is eq ba bt =
    (&&) ( eq ( node ba ) ( node bt ))
        <| List.foldl (&&) True
           ( List.map2 ( is eq ) ( kids ba ) ( kids bt ))


{-| May yield first branch for which _predicate_ holds.-}
first : ( a -> Maybe b ) -> Branch a -> Maybe b
first fu brn =
    fu ( node brn )
        |> Maybe.Extra.or ( first_just ( first fu ) ( kids brn |> List.reverse ) )
                
       

-- serial form


{-| A unique linear representation.

    singleton 5 |> serialize ( (+) 5 >> String.fromInt )
        --> "10"
-}
serialize : ( a -> String ) -> Branch a -> String
serialize from_node =
    let
        serialize_at depth brn =
            brn
                |> kids
                |> List.map ( serialize_at ( depth+1 ) )
                |> (::) ( from_node ( node brn ) )
                |> String.join ( "\n" ++ ( String.repeat (1+depth) "  " ) )
    in
        serialize_at 0

{-| Parse to string. This is a special case of [`serialize`](serialize).

    to_string = serialize identity
-}
to_string : Branch String -> String
to_string = serialize identity


            
{-| Parse back from a linear representation.

    from_string "a" |> node
        --> "a"
    "a" |> deserialize String.toUpper >> serialize String.toLower 
        --> "a"
-}
deserialize : ( String -> a ) -> String -> Branch a
deserialize to_node =
    let
        strip_empty lines =
            if ( List.head lines |> Maybe.andThen String.Extra.nonBlank ) == Nothing then
                strip_empty ( List.tail lines |> Maybe.withDefault [] )
            else lines
                
        children lines =
            List.tail lines
                |> Maybe.withDefault []
                |> List.map ( String.dropLeft 2 )
                |> segment_at ( String.startsWith "  " >> not )
        unfold lines =
            Tree.tree
                ( List.head lines |> Maybe.withDefault "" |> to_node )
                ( case lines of
                      [] -> []
                      li -> li |> children |> List.map unfold )
    in
        String.lines
            >> strip_empty >> List.reverse >> strip_empty >> List.reverse
            >> unfold


{-| Parse from string. This is a special case of [`deserialize`](deserialize).

    from_string == deserialize identity

    from_string >> to_string == identity

    to_string >> from_string == identity
-}
from_string : String -> Branch String
from_string = deserialize identity
                
{-| log to the console.
-}
log : ( a -> String ) ->
      Map ( Branch a )       
log from_node =
    let
        trace_lines ll =
            case ll of
                l::ines ->
                    multitrace l l |> always ( trace_lines ines )
                _ ->
                    ()
    in
        \brn -> brn
            |> serialize from_node
            |> String.lines >> List.reverse >> trace_lines
            |> always brn

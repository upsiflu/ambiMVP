module Compositron.Structure.EagerZipperTree exposing
    ( Structure
        
    -- create
    , singleton
        
    -- read
    , node
    , branch
    , group
    , template_group
    , tree
        
    -- navigate
    , find
    , mark

    -- map (with a primer)
    , accept_template

    -- map a Map
    , up_the_ancestry
    , up_and_down
    , each_in_group
        
    -- serial form
    , serialize, to_string
    , deserialize, from_string
    , log
    )

{-| Eagerly evaluated tree with zipper.

# Definition
@docs Structure

# Create                                                                             
@docs singleton

# Read                                                                               
@docs branch
@docs node
@docs tree
@docs group
@docs template_group

# Navigate                                                                           
@docs find
@docs mark

# Map (with a primer)             
@docs accept_template

# Map a Map                                                                          
@docs each_in_group
@docs up_the_ancestry
@docs up_and_down

# Serial form                                                                        
@docs serialize
@docs to_string
@docs deserialize
@docs from_string
@docs log
-}


        
import String.Extra

import Tree.Zipper as Zipper exposing ( Zipper )
import Tree exposing ( Tree )

import Helpers exposing (..)

import Compositron.Structure.Group as Group exposing ( Group )
import Compositron.Structure.Branch as Branch exposing ( Branch )

{-| Tree with Zipper of nodes *a*.-}
type alias Structure a =
    Zipper a





        
-- create


{-| Create a Structure from a single node.

    singleton 1 |> serialize String.fromInt == "1"
 -}
singleton : a -> Structure a
singleton = Branch.singleton >> Zipper.fromTree


            
-- read


{-| The subset of the tree, beginnning with the focus. 

    import Compositron.Structure.Branch as Branch exposing  ( Branch )
    import Helpers exposing (..)

    from_string "a\n  b\n  c"
        |> perhaps ( find ( (==) "c") )
        |> branch
    --> Branch.singleton "c"
-}
branch : Structure a -> Branch a
branch = Zipper.tree


{-| The whole tree, from root onwards. 

    import Compositron.Structure.Branch as Branch exposing  ( Branch )
    import Helpers exposing (..)

    from_string "a\n  b\n  c"
        |> perhaps ( find ( (==) "c") )
        |> tree
        |> Branch.node
    --> "a"
-}
tree : Structure a -> Branch a
tree =
    Zipper.root >> branch


{-| The node in focus. 

    singleton 1 |> node == 1                                              
-}
node : Structure a -> a
node = branch >> Branch.node

             
          

       
{-|Collect all branches in the group around the focus into a Group.

    import Helpers.LZipper as LZipper exposing  ( LZipper )
    import Helpers exposing (..)
    import Compositron.Structure.Branch as Branch exposing ( Branch )

    singleton "a" 
        |> group
        |> LZipper.map Branch.node 
    --> { before = []
    --> , focus  = "a"
    --> , after  = [] 
    --> }

    from_string "a\n  b\n    c\n  d\n    e" 
        |> perhaps ( find ( (==) "d" ) ) 
        |> group
        |> LZipper.map Branch.node
    --> { before = ["b"]
    --> , focus  =  "d"
    --> , after  = [] 
    --> }
-}
group : Structure a -> Group a
group =
    all3 ( Zipper.siblingsBeforeFocus, branch, Zipper.siblingsAfterFocus )
        >> \( b, f, a ) -> { before = b, focus = f, after = a }

                           
{-| **🅁 Template Subgroups.** Toplevel template branches are not mutual neighbours.

Collect all branches in the group around the focus into a Group. If in root or one level below, exclude the siblings.

    import Helpers.LZipper as LZipper exposing  ( LZipper )
    import Helpers exposing (..)
    import Compositron.Structure.Branch as Branch exposing ( Branch )

    from_string "a\n  b\n    c\n  d\n    e" 
        |> perhaps ( find ( (==) "d" ) )
        |> template_group
        |> LZipper.map Branch.node 
    --> { before = []
    --> , focus  = "d"
    --> , after  = [] 
    --> }

    from_string "a\n  b\n    c\n    d\n    e" 
        |> perhaps ( find ( (==) "d" ) )
        |> template_group
        |> LZipper.map Branch.node
    --> { before = ["c"]
    --> , focus  =  "d"
    --> , after  = ["e"] 
    --> }
-}

template_group : Structure a -> Group a
template_group struct =
    if
        Zipper.parent struct == Just ( Zipper.root struct )
        || Zipper.parent struct == Nothing
    then
        { before = [], focus = branch struct, after = [] }
    else
        group struct
        


            
-- navigate


{-| If possible, switch focus to the first node, beginning at root, 
that satisfies a predicate. 

    find_in_structure : ( String -> Bool ) -> Maybe String
    find_in_structure fu =
        from_string "a\n  b"
            |> find fu
            |> Maybe.map node
    
    (==) "b"    |> find_in_structure --> Just "b"
    (\_-> True) |> find_in_structure --> Just "a"
    (==) "c"    |> find_in_structure --> Nothing
-}
find : ( a -> Bool ) -> Structure a -> Maybe ( Structure a )
find = Zipper.findFromRoot


{-| Map the focused node without affecting the structure. 

    "a" |> singleton >> mark String.toUpper >> node
    --> "A"
-}
mark : Map a -> Map ( Structure a )
mark = Zipper.mapLabel       


                

{-| Merge a structure with a group of template branches. The **match** ( see [`Match`](Helpers#Match) ) between a neighbour of the focus and its corresponding **template branch** will yield

- _Keep_: keep the existing branch and continue matching the stream, or
- _Skip_: keep the existing branch but postpone the match
- _Insert_: accept the template branch, thereby iterating a **primer**, and insert it.

Excessive live branches are kept; errornous excessive template branches are discarded.

The current node will be left unchanged!
    
    import Compositron.Structure.Branch as Branch exposing ( Branch )
    import Helpers exposing (..)
    import Helpers.LZipper as LZipper exposing ( LZipper )

              
    next : Int -> String -> ( Int, String )
    next p t = ( p+1, t ++ "!" ) 

    match : Branch String
                -> Branch String 
                -> Match
    match lb tb =
            if Branch.is (==) lb tb then
                Keep
            else
                Insert

    go : LZipper String -> ( Int, Structure String ) -> ( Int, String )
    go tmp = accept_template
                next
                match
                ( tmp |> LZipper.map Branch.from_string )
            >> Tuple.mapSecond to_string

    liv0 : Structure String
    liv0 = from_string "a\n  b\n  c\n  d\n  e" |> perhaps ( find ( (==) "b" ) )

    tmp0 : LZipper String
    tmp0 = { before = [], focus = "b", after = [ "c", "d", "X", "Y" ] } 

    go tmp0 ( 0, liv0 )
    --> ( 2, "a\n  b\n  c\n  d\n  X!\n  Y!\n  e" )


    liv1 : Structure String
    liv1 = from_string "a\n  b\n    c\n" |> perhaps ( find ( (==) "c" ) )

    tmp1 : LZipper String
    tmp1 = { before = [ "<" ], focus = "b", after = [ ">" ] } 
                   
    go tmp1 ( 0, liv1 )
    --> ( 2, "a\n  b\n    <!\n    c\n    >!" )


    liv2 : Structure String
    liv2 = from_string "a\n  b\n    c\n    *" |> perhaps ( find ( (==) "*" ) )

    tmp2 : LZipper String
    tmp2 = { before = [ "<", "c" ], focus = "d", after = [ ">" ] } 
  
    go tmp2 ( 0, liv2 )
    --> ( 2, "a\n  b\n    <!\n    c\n    *\n    >!" )


    liv3 : Structure String
    liv3 = from_string "a\n  b"

    tmp3 : LZipper String
    tmp3 = { before = [ "<", "c" ], focus = "focus\n  new", after = [ ">" ] } 
  
    go tmp3 ( 0, liv3 )
    --> ( 1, "a\n  new!" )
-}
accept_template :
    ( p -> t -> ( p, a ) ) ->
    ( Branch a -> Branch t -> Match ) ->
    Group t ->
    Map ( p, Structure a )
accept_template next match template ( primer0, structure ) =
    let         
        forward :
            ( List ( Branch a ), List ( Branch t ) ) ->
            Map ( p, List ( Branch a ) )
        forward remainder ( pre, acc ) =
            case remainder of
                ( live, [] ) ->
                    ( pre, acc++live )

                ( [], t::emp ) ->
                    Branch.accept next ( pre, t )
                        |> Tuple.mapSecond ( \brn -> acc++[brn] )
                        |> forward ( [], emp )

                ( l::ive, t::emp ) ->
                     case ( match l t ) of
                         Keep ->
                         -- successful match, ditch the template branch.
                             forward ( ive, emp ) ( pre, acc++[l] )

                         Skip ->
                         -- postpone the same match to the next live branch.
                             forward ( ive, t::emp ) ( pre, acc++[l] )
                             
                         Insert ->
                         -- accept and insert the template branch.
                             Branch.accept next ( pre, t ) 
                                 |> Tuple.mapSecond ( \brn -> acc++[brn] )
                                 |> forward ( l::ive, emp )
                
        backward ( l, t ) ( pre, acc )=
            forward ( List.reverse l, List.reverse t ) ( pre, acc )
                |> Tuple.mapSecond List.reverse
        
        live_group =
            group structure

        not_root =
            Zipper.parent structure /= Nothing
                
     -- let the primer churn back and forth.
        ( primer1, back_branches ) =
            ( primer0, [] )
                |> map_if not_root ( backward ( live_group.before, template.before ))
                   
        ( primer2, kids ) =
            ( primer1, [] )
                |> forward ( [], Branch.kids template.focus )
               
        ( ender, forth_branches ) =
            ( primer2, [] )
                |> map_if not_root ( forward ( live_group.after, template.after ) )
               
    in
        ( ender
        , structure
            |> set_group
               { before = back_branches
               , focus = Branch.create ( node structure ) kids
               , after = forth_branches
               }
        )

        
set_group : Group a -> Map ( Structure a )
set_group grp structure =
    case Zipper.parent structure of
        Nothing ->
            Zipper.fromTree grp.focus
        Just p ->
            let pivot = Branch.node ( grp.focus )
                new_parent =
                    Branch.create ( node p ) ( Group.to_list grp )
            in
                p   |> Zipper.replaceTree new_parent
                    |> find ( (==) pivot )
                    |> Maybe.withDefault structure
         

        
-- map a Map


{-| Apply a Map to each branch in the group, including the focused one. -}
each_in_group : Map ( Map ( p, Structure a ) )
each_in_group fu ( primer, struct ) =
    let
        pivot =
            node struct
                
        first_sibling =
            perhaps ( Zipper.parent >> Maybe.map ( perhaps Zipper.firstChild ) ) struct

        maybe_next ( x, s ) =
            case Zipper.nextSibling s of
                Nothing -> Nothing
                Just ns -> Just ( x, ns )
    in
        ( primer, first_sibling )
            |> while_just ( fu ) maybe_next
            |> Tuple.mapSecond ( perhaps <| Zipper.findPrevious ( (==) pivot ) )



{-| Apply a Map to each focus that traces up to root.-}
up_the_ancestry : Map ( Map ( p, Structure a ) )
up_the_ancestry fu ( primer, struct ) =
    let
        pivot =
            node struct

        maybe_next ( x, cur ) =
            case Zipper.parent cur of
                Nothing -> Nothing
                Just ancestor -> Just ( x, ancestor )
    in
        ( primer, struct )
            |> while_just ( fu ) maybe_next
            |> Tuple.mapSecond ( perhaps <| Zipper.findNext ( (==) pivot ) )

               
{-| Apply a Map to each focus that traces up to root.-}
down_the_branch : Map ( Map ( p, Structure a ) )
down_the_branch fu ( primer, struct ) =
    let
        pivot =
            Zipper.fromTree ( branch struct )

        maybe_next ( x, cur ) =
            case Zipper.forward cur of
                Nothing -> Nothing
                Just successor -> Just ( x, successor )
    in
        ( primer, pivot )
            |> while_just ( fu ) maybe_next
            |> Tuple.mapSecond
               ( Zipper.root >> branch >> with struct ( Zipper.replaceTree ) ) 
            

               
{-| Apply a Map to each focus that traces up to root as well as down each kid.-}
up_and_down : Map ( Map ( p, Structure a ) )
up_and_down fu =
    down_the_branch fu >> up_the_ancestry fu


                      
-- serial form


{-| A unique linear representation. 

    singleton "a" |> serialize identity == "a" 
-}
serialize : ( a -> String ) -> Structure a -> String
serialize from_node =
        Zipper.root >> branch >> Branch.serialize from_node

{-| Special case of serialize.-}
to_string : Structure String -> String
to_string = serialize identity
            
{-| Parse back from a linear representation.
Supply a node deserializer and a check whether a given node should be focused.-}       
deserialize : ( String -> a ) -> ( a -> Bool ) -> String -> Structure a
deserialize to_node focus =
    Branch.deserialize to_node >> Zipper.fromTree >> perhaps ( find focus )

{-| Special case of deserialize.-}
from_string : String -> Structure String
from_string = deserialize identity ( always True )

{-| log to the console.
-}
log : ( a -> String ) -> Map ( Structure a )
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
                          
                          
                          
                          
  
-- tests

           {--
----0
                         
test0 =
    example0
        |> trace "Testing template imposition with simple nodes."
        |> log identity
        |> impose_template0
        |> Tuple.second
        |> log identity
        |> serialize identity
         
example0 =
    --i ( item type ) is Int
    --a ( node type ) is String
 """root
  push
  skip
  <|>
  overwrite template
  also overwrite template"""
      |> ( Just >> identity |> deserialize "" ) |> perhaps ( find ( (==) "<|>" ) )

               
impose_template0 stru =
    impose_template
        ( [ 0, 1 ], [ 2, 3, 4 ] )
        ( \live temp ->
              let
                  make_template_node = \pre -> pre ++ "," ++ ( String.fromInt temp )
              in case live of
                  Nothing -> Match ( Just make_template_node )
                  Just "skip" -> Skip
                  Just "push" -> Match ( Just make_template_node )
                  _ -> Match Nothing
        )
        ( "->", stru )


node_to_string1 ( int, str ) = ( String.fromInt int )++"."++str            
string_to_node1 str =
    case String.split "." str of
        [ n, s ] -> Just ( ( String.toInt >> Maybe.withDefault 0 ) n, s )
        _ -> Nothing
             
----1

test1 =
    example1
        |> trace "Testing template imposition with tuply nodes."
        |> log node_to_string1
        |> trace "imposing template [ <<, < ],[ >, >>, >>> ]"
        |> trace "    skip -> accept the live branch, defer the template branch"
        |> trace "    push -> accept the template branch, defer the live branch."
        |> trace "     ... -> accept the live branch instead of the template branch."
        |> impose_template1
        |> Tuple.second
        |> log node_to_string1
        |> trace "We see that the index first grows above the pivot, then below,"
        |> trace "and that it only increases for each new (template) insertion."

           
example1 =
    --i ( item type ) is String
    --a ( node type ) is ( item, String )
 """0.root
  1.push
  2.skip
  3.<|>
  4.A
  5.skip
  6.push
  7.C"""
      |> deserialize ( 0, "" ) string_to_node1
      |> perhaps ( find ( Tuple.second >> ( (==) "<|>" ) ) )

         
impose_template1 stru =
    impose_template
        ( [ "<<", "<" ], [ ">", ">>", ">>>" ] )
        ( \live temp ->
              let
                  make_template_node pre =
                      case pre of
                          ( n, s ) -> ( n+1, temp )   
              in case live of
                  Nothing -> Match ( Just make_template_node )
                  Just ( _, "skip" ) -> Skip
                  Just ( _, "push" ) -> Match ( Just make_template_node )
                  _ -> Match Nothing
        )
        ( ( 99, "->" ), stru )
--}
           
----2
{-- DEFUNCT due to refactoring

test2 =
    example1
        |> trace "Testing branch replacement."
        |> log node_to_string1
        |> replace_branch2
        |> trace "Mapping head, and replacing children with a..e."
        |> trace "In this example, new nodes get an increasing index."
        |> Tuple.second
        |> log node_to_string1



           {-
replace_branch :
    List i ->
    ( i -> a -> a ) ->
    Map ( a, Structure a )
replace_branch ( head, forest ) match ( primer, struct ) =
-}

replace_branch2 stru =
    let
        next_node new ( n, itm ) =
            ( (n+1), new )        
    in replace_branch
        ( stru |> node |> Tuple.mapSecond ( (++) "mapped head -> " )
        , [ "a", "b", "c", "d", "e" ] )
        next_node
        ( ( 7, "C" ), stru )


test3 =
    test1
        |> replace_branch2
        |> Tuple.second
        |> trace "Now appending a tree replacement.."
        |> log node_to_string1

replace_branch4 ( primer, stru ) =
    let
        next_node new ( n, itm ) =
            ( (n+1), new )        
    in replace_branch
        ( stru |> node |> Tuple.mapSecond ( (++) "mapped node -> " )
        , [ "a", "b", "c", "d", "e" ] )
        next_node
        ( primer, stru )

test4 =
    example1
        |> trace "Testing a continuous primer."
        |> log node_to_string1
        |> replace_branch2
        |> trace "Mapping head, and replacing children with a..e."
        |> trace "In this example, the primer is retained between"
        |> trace "subsequent operations for a continuously increasing index."
        |> Tuple.mapSecond ( find ( Tuple.second >> ((==) "c") ) |> perhaps )
        |> replace_branch4
        |> Tuple.mapSecond ( find ( Tuple.first >> ((<=) 16 ) ) |> perhaps )
        |> replace_branch4
        |> Tuple.second
        |> log node_to_string1


--}
           
----4
{--

example5 =
    --i ( item type ) is String
    --a ( node type ) is ( item, String )
 """0.root
  1.<->
  2.skip
  3.<|>
  4.A
  5.skip
  6.<->
  7.<->
  8.push
  9.<->"""
      |> deserialize ( 0, "" ) string_to_node1
      |> perhaps ( find ( Tuple.second >> ( (==) "<|>" ) ) )


impose_template5 =
    impose_template
        ( [ "<<", "<" ], [ ">", ">>", ">>>" ] )
        ( \live temp ->
              let
                  make_template_node pre =
                      case pre of
                          ( n, s ) -> ( n+1, temp )   
              in case live of
                  Nothing -> Match ( Just make_template_node )
                  Just ( _, "skip" ) -> Skip
                  Just ( _, "push" ) -> Match ( Just make_template_node )
                  _ -> Match Nothing
        )

--each_sibling : ( Map ( a, Structure a ) ) -> Map ( a, Structure a )
--each_sibling fu ( primer, struct ) =

impose_all_templates5 structure =
    let
        modify ( pre, struct ) =
            case node struct of
                ( _, "<->" ) -> impose_template5 ( pre, struct )
                _            -> ( pre, struct )
    in
        each_in_group modify ( ( 99, "->" ), structure )


test5 =
    example5
        |> trace "Testing multiple template impositions via fold_group."
        |> log node_to_string1
        |> impose_all_templates5
        |> trace "Template impositions at each '<->'."
        |> Tuple.second
        |> log node_to_string1
        |> trace "Note that in this ruleset, '<->' replaces the template."
--}

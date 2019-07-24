module Compositron.Structure.EagerZipperTree exposing
    ( Structure
    , Branch
        
    -- create
    , singleton
        
    -- read
    , node
    , branch
        
    -- map
    , find
    , map_group
    , fold_group
    , replace_branch
    , impose_template
        
    -- string
    , serialize
    , deserialize
        
    -- testing
    , example0
    , test0
    , example1
    , test1
    , test2
    , test3
    , test4
    , test5
    )

import String.Extra

import Tree.Zipper as Zipper exposing ( Zipper )
import Tree exposing ( Tree )

import Helpers exposing (..)

type alias Structure a =
    Zipper a

type alias Branch a =
    Tree a

        
-- create

singleton : a -> Structure a
singleton = Tree.singleton >> Zipper.fromTree


            
-- read

node : Structure a -> a
node = branch >> bode

bode : Branch a -> a
bode = Tree.label

find : ( a -> Bool ) -> Structure a -> Maybe ( Structure a )
find = Zipper.findFromRoot


-- edit branches before inserting them

branch : Structure a -> Branch a
branch = Zipper.tree

bringleton = Tree.singleton
         
       
       
-- map


map_group : Map ( List ( Branch a ) ) -> Map ( Structure a )
map_group fu struct =
    Zipper.parent struct
        |> Maybe.map
           ( Zipper.tree
                 >> Tree.mapChildren fu
                 >> Zipper.fromTree
                 >> perhaps ( Zipper.findNext ( (==) ( node struct ) ) )
           )
        |> Maybe.withDefault
           ( fu [ branch struct ]
           |> List.head
           |> Maybe.map Zipper.fromTree
           |> Maybe.withDefault struct
           )


fold_group : ( Map ( a, Structure a ) ) -> Map ( a, Structure a )
fold_group fu ( primer, struct ) =
    let
        pivot =
            node struct
                
        first_sibling : Map ( Structure a )
        first_sibling =
            perhaps ( Zipper.parent >> Maybe.map ( perhaps Zipper.firstChild ) )

        maybe_next ( x, s ) =
            case Zipper.nextSibling s of
                Nothing -> Nothing
                Just ns -> Just ( x, ns )
    in
        ( primer, first_sibling struct )
            |> while_just ( fu ) maybe_next
            |> Tuple.mapSecond ( perhaps <| Zipper.findPrevious ( (==) pivot ) )

                
replace_branch :
    ( a, List i ) ->
    ( a -> i -> a ) ->
    Map ( a, Structure a )
replace_branch ( head, forest ) match ( primer, struct ) =
    let
        ( ender, children ) =
            forest
                |> List.foldl
                   ( \itm ( pre, acc ) ->
                         match pre itm |> both ( identity, bringleton >> after acc )
                   ) ( primer, [] )
    in
        ( ender
        , struct
            |> Zipper.replaceTree
               ( Tree.tree head children )
        )

               
impose_template :
    ( List i, List i ) ->
    ( Maybe a -> i -> Skippable ( Maybe ( a -> a ) ) ) ->
    Map ( a, Structure a )
impose_template ( temp_before, temp_after ) match ( primer, structure ) =
    let
        ( live_before, live_after ) =
            structure |> both ( Zipper.siblingsBeforeFocus, Zipper.siblingsAfterFocus )
                
        forward :
            ( a, List ( Branch a ) ) ->
            ( List ( Branch a ), List i ) ->
            ( a, List ( Branch a ) )
        forward ( pre, acc ) remainder =
            case remainder of
                -- List ( Branch a ), List i
                ( live, t::emp ) ->
                    let ( maybe_l, maybe_ive ) =
                            both ( List.head >> Maybe.map ( \x -> [x] ), List.tail ) live
                        ( l, ive ) =
                            each ( Maybe.withDefault [] ) ( maybe_l, maybe_ive )
                                
                    in case match ( live |> List.head |> Maybe.map bode ) t of
                        Skip ->
                            -- accept the live branch.
                            forward ( pre, acc++l ) ( ive, t::emp )
                                
                        Match Nothing ->
                            -- successful match, discard the template branch.
                            forward ( pre, acc++l ) ( ive, emp )
                                
                        Match ( Just make_template_node ) ->
                            -- accept the template branch.
                            let new = pre |> make_template_node
                            in forward ( new, acc++[( bringleton new )] ) ( live, emp )
                            
                ( live, [] ) -> ( pre, acc++live )

        backward ( pre, acc ) ( l, t ) =
            forward ( pre, acc ) ( List.reverse l, List.reverse t )
                |> Tuple.mapSecond List.reverse
                   
        ( follower, back_branches ) =
            backward ( primer, [] ) ( live_before, temp_before )
        ( ender, forth_branches ) =
            forward ( follower, [] ) ( live_after, temp_after )
    in
        ( ender
        , map_group
            ( \_-> back_branches ++ [ branch structure ] ++ forth_branches )
            structure
        )
 

         
-- serialize and deserialize

serialize : ( a -> String ) -> Structure a -> String
serialize from_node =
    let
        serialize_at depth structure =
            structure
                |> branch >> Tree.children >> List.map Zipper.fromTree
                |> List.map ( serialize_at ( depth+1 ) )
                |> (::) ( from_node ( node structure ) )
                |> String.join ( "\n" ++ ( String.repeat (1+depth) "  " ) )
                
    in
        Zipper.root
        >> serialize_at 0


            
deserialize : a -> ( String -> Maybe a ) -> String -> Structure a
deserialize default to_node =
    let
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
            >> unfold
            >> Tree.map ( Maybe.withDefault default )
            >> Zipper.fromTree



log from_node =
    let
        trace_lines ll =
            case ll of
                l::ines -> Debug.log "::" l |> always ( trace_lines ines )
                _ -> ()

    in \str ->
        str
            |> serialize from_node
            |> String.lines >> List.reverse >> trace_lines
            |> always str

            
-- tests

           
trace text = \x -> Debug.log "🛈" text |> always x
             
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

           
----2


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
    ( Map i, List i ) ->
    ( a -> i -> a ) ->
    Map ( a, Structure a )
replace_branch ( head, forest ) match ( primer, struct ) =
-}

replace_branch2 stru =
    let
        next_node ( n, itm ) new =
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
        next_node ( n, itm ) new =
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



           
----4


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

--fold_group : ( Map ( a, Structure a ) ) -> Map ( a, Structure a )
--fold_group fu ( primer, struct ) =

impose_all_templates5 structure =
    let
        modify ( pre, struct ) =
            case node struct of
                ( _, "<->" ) -> impose_template5 ( pre, struct )
                _            -> ( pre, struct )
    in
        fold_group modify ( ( 99, "->" ), structure )


test5 =
    example5
        |> trace "Testing multiple template impositions via fold_group."
        |> log node_to_string1
        |> impose_all_templates5
        |> trace "Template impositions at each '<->'."
        |> Tuple.second
        |> log node_to_string1
        |> trace "Note that in this ruleset, '<->' replaces the template."
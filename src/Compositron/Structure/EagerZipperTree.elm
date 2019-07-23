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
    , replace_branch
    , grow_branches
    , impose_template
        
    -- string
    , serialize
    , deserialize
        
    -- testing
    , example0
    , test0
    , example1
    , test1
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


replace_branch : Branch a -> Map ( Structure a )
replace_branch =
     Zipper.replaceTree


grow_branches : ( a -> Nonempty ( Branch a ) ) ->
                Map ( Structure a )
grow_branches transform structure =
    let
        ( head, tail ) =
            node structure |> transform
        append siblings =
            \s-> List.foldr ( Zipper.append ) s siblings
    in
        structure
            |> Zipper.replaceTree head
            |> append tail


-- A simple replacement of data might yield ( map_item.., [] )
-- whereas a more complex modification such as field insertion
-- will yield a head (constant sig) and scaling children.
-- If there is a mix of insertions and reinsertions, we will
-- bundle the reinsertions with the insertion to get a ???
               
{-| template imposition

This imposes the branches of the template around the target
according to a comparison function between each live and tamplate branches.

    (1) impose_template t m >> impose_template t m === impose_template t

template: a tuple of branch lists that go before, resp. after the target.

match: compares the live item with the template item at the corresponding position.
    A result of Skip means, stretch the template and keep the live item.
    A Match means, keep the live item and discard the template item.
    No Match, replace the live item by the template item.


Example

    i ( item type ) is Int
    a ( node type ) is String

    impose_template
        ( [ 1 ], [ 2, 3 ] )
        ( \live temp ->
              let make_template_node = \pre -> pre ++ ", " ++ ( String.fromInt temp )
              in case live of
                  Nothing -> Match ( Just make_template_node )
                  Just "?" -> Skip
                  Just "-" -> Match ( Just make_template_node )
                  _ -> Match Nothing
        )
        "primer"

Result

input -> template -> output       mutation

root                 root                    clear_children
  B          1         B            =        keep_child B
  here       *         A            +A       
  ?          2         here         x
  B          3         ?            =
    keep (B)           B            =
  A                       keep (B)
    move (A)           C            +C
  -                    A            =
  B                       move (A)
                       -            =
                       B            =

|-}




               
impose_template : ( List i, List i ) ->
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

        map_group : Map ( List ( Branch a ) ) -> Map ( Structure a )
        map_group fu stru =
            Zipper.parent stru
                |> Maybe.map
                   ( Zipper.tree
                     >> Tree.mapChildren fu
                     >> Zipper.fromTree
                     >> perhaps ( Zipper.findNext ( (==) ( node stru ) ) )
                   )
                |> Maybe.withDefault stru
                   
        ( seconder, back_branches ) =
            backward ( primer, [] ) ( live_before, temp_before )
        ( tertier, forth_branches ) =
            forward ( seconder, [] ) ( live_after, temp_after )
    in
        ( tertier
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
                l::ines -> Debug.log ":" l |> always ( trace_lines ines )
                _ -> ()

    in \str ->
        str
            |> serialize from_node
            |> String.lines >> List.reverse >> trace_lines
            |> always str

            
-- tests

           
trace text = \x -> Debug.log "🛈" text |> always x
             
----0
                         
test0 = example0 |> impose_template0 |> Tuple.second |> serialize identity
         
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

test1 = example1
      |> log node_to_string1
      |> trace "imposing template [ <<, < ],[ >, >>, >>> ]"
      |> trace "    skip -> accept the live branch, defer the template branch"
      |> trace "    push -> accept the template branch, defer the live branch."
      |> trace "     ... -> accept the live branch instead of the template branch."
      |> impose_template1
      |> Tuple.second
      |> log node_to_string1
      |> serialize ( node_to_string1 )

example1 =
 """0.root
  1.push
  2.skip
  3.<|>
  4.A
  5.B
  6.push
  7.C"""
      |> deserialize ( 0, "" ) string_to_node1
      |> perhaps ( find ( Tuple.second >> ( (==) "<|>" ) ) )

               
--deserialize : a -> ( String -> Maybe a ) -> String -> Structure a
--deserialize default to_node =
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
        ( ( 0, "->" ), stru )

           

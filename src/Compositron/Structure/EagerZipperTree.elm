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
    , example
    , test0
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
                  a ->
                  Map ( Structure a )
impose_template ( temp_before, temp_after ) match primer structure =
    let
        ( live_before, live_after ) =
            structure |> both ( Zipper.siblingsBeforeFocus, Zipper.siblingsAfterFocus )
                
        forward :
            ( List ( Branch a ), a ) ->
            ( List ( Branch a ), List i ) ->
            List ( Branch a )
        forward ( acc, pre ) remainder =
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
                            l ++ ( forward ( acc, pre ) ( ive, t::emp ) )
                                
                        Match Nothing ->
                            -- successful match, discard the template branch.
                            l ++ ( forward ( acc, pre ) ( ive, emp ) )
                                
                        Match ( Just make_template_node ) ->
                            -- accept the template branch.
                            let new = pre |> make_template_node
                            in ( bringleton new ) :: ( forward ( acc, new ) ( live, emp ) )
                            
                ( live, [] ) -> acc ++ live

        backward ( acc, pre ) ( l, t ) =
            forward ( acc, pre ) ( List.reverse l, List.reverse t ) |> List.reverse

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
                   
    in
        map_group
            ( \_->
                  ( backward ( [], primer ) ( live_before, temp_before ) )
                  ++
                  [ branch structure ]
                  ++
                  ( forward ( [], primer ) ( live_after, temp_after ) )
            )
            structure
 

         
-- serialize and deserialize

serialize : ( a -> String ) -> Structure a -> String
serialize serializer =
    let
        serialize_at depth structure =
            structure
                |> branch >> Tree.children >> List.map Zipper.fromTree
                |> List.map ( serialize_at ( depth+1 ) )
                |> (::) ( serializer ( node structure ) )
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


                
-- tests

example =
 """root
  B
  here
  ?
  B
    keep (B)
  A
    move (A)
  -
  B""" |> ( Just >> identity |> deserialize "" ) |> perhaps ( find ( (==) "here" ) )

               
test0 =
    let
        st = Just >> identity |> deserialize ""

    in impose_template
        ( [ 1 ], [ 2, 3 ] )
        ( \live temp ->
              let
                  make_template_node = \pre -> pre ++ ", " ++ ( String.fromInt temp )
              in case live of
                  Nothing -> Match ( Just make_template_node )
                  Just "?" -> Skip
                  Just "-" -> Match ( Just make_template_node )
                  _ -> Match Nothing
        )
        "primer"
            
    --i ( item type ) is Int
    --a ( node type ) is String


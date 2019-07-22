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

       
       
-- map


replace_branch : Branch a ->
              Map ( Structure a )
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



               
{-| impose template

This imposes the branches of the template around the target
according to a comparison function between each live and tamplate branches.

    (1) impose_template t m >> impose_template t m === impose_template t

template: a tuple of branch lists that go before, resp. after the target.

match: compares the live item with the template item at the corresponding position.
    A result of Skip means, stretch the template and keep the live item.
    A Match means, keep the live item and discard the template item.
    No Match, replace the live item by the template item.


Example

    impose_template
        ( [ br "A" ], [ br "B", br "C" ] )
        ( \live temp -> 
              if live == "?" 
              then Debug.log "skipping" Skip
              else Match <| Debug.log "is matching?" ( live == temp )
        )

Result

input -> template -> output

root                 root
  B          A         B
  here       *         A
  ?          B         here
  B          C         ?
    keep (B)           B
  A                       keep (B)
    move (A)           C
  -                    A
  B                       move (A)
                       -
                       B

|-}
               
impose_template : ( List ( Branch a ), List ( Branch a ) ) ->
                  ( a -> a -> Skippable Bool ) ->
                  Map ( Structure a )
impose_template ( temp_before, temp_after ) match structure =
    let
        ( live_before, live_after ) =
            structure |> both ( Zipper.siblingsBeforeFocus, Zipper.siblingsAfterFocus )

        match_forward : List ( Branch a ) ->
                      ( List ( Branch a ), List ( Branch a ) ) ->
                      List ( Branch a )
        match_forward acc remainder =
            case remainder of
                ( [], temp ) -> acc ++ temp
                ( live, [] ) -> acc ++ live
                ( l::ive, t::emp ) ->
                    case match ( bode l ) ( bode t ) of
                        Skip ->
                            l :: ( match_forward acc ( ive, t::emp ) )
                        Match True ->
                            l :: ( match_forward acc ( ive, emp ) )
                        Match False ->
                            t :: ( match_forward acc ( l::ive, emp ) )

        match_backward acc remainder =
            match_forward acc ( each List.reverse remainder ) |> List.reverse

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
                  ( match_backward [] ( live_before, temp_before ) )
                  ++
                  [ branch structure ]
                  ++
                  ( match_forward [] ( live_after, temp_after ) )
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
        br = st >> branch

    in
        impose_template
           ( [ br "A" ], [ br "B", br "C" ] )
           ( \live temp -> 
                 if live == "?" 
                 then Debug.log "skipping" Skip
                 else Match <| Debug.log "is matching?" ( live == temp )
           )
           

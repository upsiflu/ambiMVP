module Helpers.LZipper exposing (..)

from_list : List a -> Maybe ( LZipper a )
from_list list =
    case list of
        l::ist ->
            Just { before = [], focus = l, after = ist }
        _ ->
            Nothing

flatten : LZipper ( LZipper a ) -> LZipper a
flatten lz =
    { before = ( List.map to_list lz.before |> List.concat ) ++ (lz.focus |> .before)
    , focus = (lz.focus |> .focus)
    , after = (lz.focus |> .after ) ++ ( List.map to_list lz.after |> List.concat )
    }
        
to_list : LZipper a -> List a
to_list lz =
    lz.before ++ [lz.focus] ++ lz.after
                
type alias LZipper a =
    { before : List a, focus: a, after: List a }
                        
map : ( a -> b ) -> LZipper a -> LZipper b
map fu lza =
    { before = List.map fu lza.before
    , focus = fu lza.focus
    , after = List.map fu lza.after
    }

singleton x = { before = [], focus = x, after = [] }

from_nonempty ( head, tail ) = { before = [], focus = head, after = tail }
                                     

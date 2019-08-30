module Compositron.Item exposing
    ( Item (..)

    -- create
    , default_symbol
        
    -- read
    , data
    , to_symbol
    , is_self_assumption
    , is_assumption
    , equal
    , alternatives
    , assumptions
        
    -- map
    , set_data
    , accept
    , freeze
    , unfreeze
        
    -- view
    , view
          
    -- serial form
    , deserialize
    , serialize
    )

{-|
# Definition
@docs Item

# create
@docs default_symbol
@docs accept

# Read
@docs data
@docs to_symbol

@docs is_self_assumption
@docs is_assumption
@docs equal
        
@docs alternatives
@docs assumptions
        
# Map
@docs set_data

@docs freeze
@docs unfreeze
        
# view
@docs view
          
# serial form
@docs deserialize
@docs serialize
-}
        
import Helpers exposing (..)
import Maybe.Extra
import Html exposing ( Html )

import Compositron.Data as Data exposing ( Data )
import Compositron.Signature as Signature exposing ( Signature )

import Compositron.View as View exposing ( View, Action (..) )
import Compositron.Flow as Flow exposing ( Flow (..) )
import Compositron.Cogroup as Cogroup exposing ( Cogroup (..) )


{-|-}
type Item l t
    = Assume ( Ambiguation ( Cogroup t ) )
    | Body String Flow
    | Err String

      
-- create


{-| creates a generic symbol to represent choices where the intended group has no symbol.-}
default_symbol : Item l t
default_symbol = Body "*" <| Flow.symbolic


primer : Item l t
primer = Err "primer"


initial : Item l t
initial = Err "root"

          
{-|-}     
accept : Item t t -> Item l t
accept itm =
    case itm of
        Assume ( ambiguation ) -> Assume ( ambiguation )
        Body n f -> Body n f
        Err str -> Err str
          
         
-- read


{-|-}
data : Item l t -> Maybe Data
data = flow >> Maybe.andThen Flow.data

{-|-}
flow : Item l t -> Maybe Flow
flow itm =
    case itm of
        Body n f -> Just f
        _ -> Nothing


{-| Maybe this item is a `Flow.Symbolic` Body?-}
to_symbol : Item l t -> Maybe ( Item l t )
to_symbol itm =
    flow itm |> Maybe.andThen ( \f -> if Flow.is_symbolic f then Just itm else Nothing )

{-|-}
is_self_assumption : Item l t -> Bool
is_self_assumption itm =
    itm == Assume ( Of Self )

{-|-}
is_assumption : Item l t -> Bool
is_assumption itm =
    case itm of
        Assume _ -> True
        _ -> False

{-|-}
alternatives : Item l t -> List ( Cogroup t )
alternatives itm =
    case itm of
        Assume a ->
            ambiguation_to_list a
        _ -> []

{-|-}
assumptions : t -> Item l t -> List ( List t )
assumptions prototype =
    alternatives >> List.map ( Cogroup.assumptions prototype )

{-|-}
equal : Item l t -> Item t t -> Bool
equal live_itm temp_itm =
    live_itm == accept temp_itm        
        
             
       
-- map


{-|-}
set_data : Data -> Map ( Item l t )
set_data = Flow.set_data >> map_flow 

       
{-|-}
map_flow : Map Flow -> Map ( Item l t )
map_flow fu itm =
    case itm of
        Body n flw -> Body n ( fu flw )
        x -> x
             
{-|-}
freeze : Map ( Item l t )
freeze =
    map_flow Flow.freeze
    
{-|-}
unfreeze : Map ( Item l t )
unfreeze =
    map_flow Flow.unfreeze
                                   


        


-- present




-- serial form



{-|-}
serialize : ( l -> String ) -> ( t -> String ) -> Item l t -> String
serialize from_l from_t itm =
        case itm of
            Assume a ->
                "<" ++ ( ambiguation_to_list a
                       |> List.map ( Cogroup.serialize from_t) |> String.join " |"
                       )
                    ++ "\t "        
            Body n f ->
                n ++ "\t" ++ Flow.serialize f
            Err s -> "⚠ " ++ s ++ "\t "

          
{-|-}
deserialize :
    ( String -> Maybe l ) ->
    ( String -> Maybe t ) ->
    String ->
        Item l t
deserialize to_l to_t str =
        case String.words str
            |> both
               ( List.head >> Maybe.withDefault ""
               , List.tail >> Maybe.withDefault [""] >> String.join " " )
        of
            ( "<", rest ) ->
                rest |> String.split " | "
                     |> List.map ( Cogroup.deserialize to_t >> Maybe.withDefault Self )
                     |> to_nonempty |> Maybe.withDefault ( Self, [] )
                     |> ambiguation_from_nonempty
                     |> Assume
                       
            ( "⚠", rest ) ->
                Err rest

            _ ->
                case String.split "\t" str
                    |> List.reverse
                    |> both
                       ( List.head >> Maybe.andThen ( Flow.deserialize )
                       , List.tail >> Maybe.map ( List.reverse >> String.join "" )
                       )
                of
                    ( Just f, Just n ) ->
                        Body n f
                    _ -> "Unable to deserialize " ++ str ++ " to item" |> Err




-- view
                      
{-|-}
view :
    Item l t
    -> Map ( View msg l t Data )
view itm =
    case itm of
        Assume tmp ->
            View.add_class "Assume"
                >> View.set_size View.Zero
                >> case tmp of
                       Of single ->
                           Cogroup.view single
                       Or head more ->
                           View.set_element
                               ( \att chi -> Html.button att [ Html.ul [] chi ] )
                               >> View.add_class "options"
        Body n f ->
            View.add_class n
                >> View.set_text n
                >> Flow.view f
                        
        Err string ->
            View.add_class string
                >> View.add_class "Err"
                

module Compositron.Item exposing
    ( Item (..)

    -- create
    , accept
          
    -- read
    , option_face
    , to_symbol
    , is_self_assumption
    , is_assumption
    , equal
    , options
    , data
        
    -- map
    , freeze
    , unfreeze
    , map_data
        
    -- view
    , view
          
    -- serial form
    , deserialize
    , serialize
    )

{-|
# Definition
@docs Item

# Create
@docs accept


# Map

@docs map_data
@docs freeze
@docs unfreeze

# Read

## A

@docs option_face
@docs to_symbol

@docs is_self_assumption
@docs is_assumption
@docs equal
        
@docs options

## D

@docs data
        
# View
@docs view
          
# Serial Form
@docs deserialize
@docs serialize
-}
        
import Helpers exposing (..)
import Helpers.Nonempty as Nonempty exposing ( Nonempty )
import Helpers.Ambiguation as Ambiguation exposing ( Ambiguation (..) )

import Compositron.Data as Data exposing ( Data )
import Compositron.Role as Role exposing ( Role )
import Compositron.Cogroup as Cogroup exposing ( Cogroup (..) )
import Compositron.Arrow as Arrow exposing ( Arrow (..) )

import Compositron.View as View exposing ( View )




{-|-}
type Item l t
    = Assume ( Ambiguation ( Cogroup t ) )
    | Body String Role
  --| Charge ( Ambiguation ( Cogroup l ) )
    | Define Data
    | Error String

      
      
-- create


primer : Item p t
primer = Error "primer"


initial : Item l t
initial = Error "root"

          
{-|-}     
accept : Item t t -> Item l t
accept itm =
    case itm of
        Assume ( ambiguation ) -> Assume ( ambiguation )
        Body n r -> Body n r
        Define dat -> Define dat
        Error message -> Error message

                   
         
-- read


{-|-}
data : Item l t -> Maybe Data
data itm =
    case itm of
        Define d -> Just d
        _ -> Nothing

{-|-}
role : Item l t -> Maybe Role
role itm =
    case itm of
        Body n r -> Just r
        _ -> Nothing


{-| While this item is in template, it shows this string as a 'face'.-}
option_face : Item l t -> Maybe String
option_face itm =
    case itm of
        Assume _ ->
            Nothing
        Body n r ->
            Just n
        Define d ->
            Just ( Data.serialize_constructor d )
        Error _ ->
            Just "⚠"
              
{-| Body with _symbolic_ Role.-}
to_symbol : Item l t -> Maybe ( Item l t )
to_symbol itm =
    role itm |> Maybe.andThen ( \r -> if Role.is_symbolic r then Just itm else Nothing )

assumption itm =
    case itm of
        Assume a -> Just a
        _ -> Nothing

{-|-}
is_assumption : Item l t -> Bool
is_assumption =
    assumption >> (/=) Nothing

{-|-}
is_self_assumption : Item l t -> Bool
is_self_assumption =
    options >> Maybe.andThen Nonempty.just_singleton >> Maybe.map Cogroup.is_self >>
        Maybe.withDefault False
        
{-|`Just` the cogroup(s) that may be chosen, or `Nothing` if item is not Assumption.-}
options : Item l t -> Maybe ( Nonempty ( Cogroup t ) )
options =
    assumption >> Maybe.map Ambiguation.to_nonempty

{-|-}
equal : Item l t -> Item t t -> Bool
equal live_itm temp_itm =
    live_itm == accept temp_itm        
        
             
       
-- map

       
{-|-}
map_data : Map Data -> Map ( Item l t )
map_data fu itm =
    case itm of
        Define d -> Define ( fu d )
        x -> x
             
{-|-}
freeze : Map ( Item l t )
freeze =
    map_data Data.freeze
    
{-|-}
unfreeze : Map ( Item l t )
unfreeze =
    map_data Data.unfreeze
                                   



-- serial form



{-|-}
serialize : ( l -> String ) -> ( t -> String ) -> Item l t -> String
serialize from_l from_t itm =
        case itm of
            Assume a ->
                "<" ++ ( Ambiguation.to_list a
                       |> List.map ( Cogroup.serialize from_t) |> String.join " |"
                       )
                    ++ "\t "        
            Body n r ->
                n ++ "\t" ++ Role.serialize r
            Define d ->
                Data.serialize d
            Error s -> "⚠ " ++ s ++ "\t "

          
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
                     |> List.map ( Cogroup.deserialize to_t )
                     |> fold_must -- discard if any error
                     |> Maybe.andThen Nonempty.from_list
                     |> Maybe.map ( Ambiguation.from_nonempty >> Assume )
                     |> Maybe.withDefault ( Error "Invalid Assumption (Codomain format)" ) 
                       
            ( "⚠", rest ) ->
                rest |> Error


            _-> case Data.deserialize str of
                    Just d ->
                        Define d

                    _-> case String.split "\t" str
                          |> List.reverse
                          |> both
                             ( List.head >> Maybe.andThen ( Role.deserialize )
                             , List.tail >> Maybe.map ( List.reverse >> String.join "" )
                             )
                        of
                            ( Just r, Just n ) ->
                                Body n r

                            _-> Error <| "Unable to deserialize " ++ str ++ " to item"
                                 




-- view
                      
{-|-}
view :
    t
    -> Item l t
    -> Map ( View node t )
view prototype itm =
    case itm of
        Assume ( Of special ) ->
            View.indicate special
                
        Assume ambi ->
            maybe View.offer ( options itm )
                    
        Body n r ->
            View.play n r

        Define d ->
            View.generate d
                
        Error message ->
            View.report message
                

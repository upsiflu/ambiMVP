module Compositron.Data exposing
    ( Data (..)
    , serialize_constructor
    , size
    , merge
    , freeze, unfreeze
    , serialize, deserialize
    , view
    )

{-|
# Create
@docs Data

# Read
@docs size

# Map
@docs merge
@docs freeze
@docs unfreeze

# Serial Form
@docs serialize
@docs deserialize
@docs serialize_constructor

# View
@docs view
-}

import Html exposing ( Html )
import Helpers exposing (..)

{-|-}
type Data
    = Text { fluid : Maybe String, frozen : Maybe String }
    | Url { href : Maybe String } 
    | Picture { src : Maybe String }
    | Youtube { id : Maybe String }
    | Vimeo { id : Maybe String }
    | Info { phrase : Maybe String }


-- read
size : Data -> Int
size d =
    case d of
        Text { fluid, frozen } ->
            String.length ( ( enstring fluid )++( enstring frozen ) )
        Info _ ->
            0
        _ ->
            20

      
-- map

{-|-}
merge : String -> Maybe Data -> Data
merge str d =
    case d of
        Just ( Text t ) ->
            Text { t | fluid = destring str }
        Just ( Url u ) ->
            Url { href = destring str }
        Just ( Picture p ) ->
            Picture { src = destring str }
        Just ( Youtube y ) ->
            Youtube { id = destring str }
        Just ( Vimeo v ) ->
            Vimeo { id = destring str }
        Just ( Info i ) ->
            Info { phrase = destring str }
        Nothing -> Info { phrase = Just ( "Your input: "++str ) }

{-|-}
freeze : Map Data
freeze d =
    case d of
        Text txt -> Text { txt | frozen = txt.fluid }
        _ -> d

{-|-}
unfreeze : Map Data
unfreeze d =
    case d of
        Text txt -> Text { txt | fluid = txt.frozen }
        _ -> d

      
           

-- serial form

enstring : Maybe String -> String
enstring = Maybe.withDefault ""
destring : String -> Maybe String
destring str = if str == "" then Nothing else Just str




{-|-}
serialize_constructor : Data -> String
serialize_constructor d =
    case d of
        Text _ ->
            "𝑇"
        Url _ ->
            "☍"
        Picture _ ->
            "🖽"
        Youtube _ ->
            "y"
        Vimeo _ ->
            "v"
        Info _ ->
            "🛈"
               
{-|-}
serialize : Data -> String
serialize d =
    serialize_constructor d
        ++case d of
              Text t ->
                  enstring t.fluid++" ~ "++enstring t.frozen
              Url u ->
                  enstring u.href
              Picture p ->
                  enstring p.src
              Info s ->
                  enstring s.phrase
              _ ->
                  "(TODO)"

{-|-}
deserialize str =
    case String.split " " str
        |> both
           ( List.head >> enstring, List.tail >> Maybe.map ( String.join " " ) >> enstring )
    of
        ( "𝑇", rest ) ->
            rest |> String.split " ~ "
                 |> list_to_tuple
                 |> Maybe.map
                    ( each destring >> \( flu, fro ) -> Text { fluid = flu, frozen = fro } )
        ( "🖽", rest ) ->
            Picture { src = destring rest } |> Just
        ( "☍", rest ) ->
            Url { href = destring rest } |> Just
        ( "🛈", rest ) ->
            Info { phrase = destring rest } |> Just
        ( _, _ ) ->
            Nothing
                        



-- View


{-|-}
view : Data -> List ( Html msg )
view d =
    case d of
        Text t ->
            [ Html.text ( enstring t.frozen ) ]
        Url _ ->
            [ Html.text "☍" ]
        Picture _ ->
            [ Html.text "🖽" ]
        Youtube _ ->
            [ Html.text "y" ]
        Vimeo _ ->
            [ Html.text "v" ]
        Info _ ->
            [ Html.text "🛈" ]
        


                

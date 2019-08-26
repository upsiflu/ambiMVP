module Compositron.Item exposing
    ( Item (..)
    , Flow

    -- create
    , default_symbol
        
    -- read
    , data
    , to_symbol
        
    , is_self_assumption
    , equal
        
    , alternatives
    , assumptions
        
    -- map
    , accept
    , set_data
    , freeze
    , unfreeze
        
    -- view
    , view
    , view_cogroup
          
    -- serial form
    , deserialize
    , serialize
    )

{-|
# Definition
@docs Item

# create
@docs default_symbol

# Read
@docs data
@docs to_symbol
        
@docs is_self_assumption
@docs equal
        
@docs alternatives
@docs assumptions
        
# Map
@docs accept
@docs set_data
@docs freeze
@docs unfreeze
        
# view
@docs view
@docs view_cogroup
@docs Flow
          
# serial form
@docs deserialize
@docs serialize
-}
        
import Html exposing ( Html )
import Html.Attributes as Attributes

import Helpers exposing (..)

import Compositron.Data as Data exposing ( Data )
import Compositron.Signature as Signature exposing ( Signature )

import Compositron.View as View exposing ( View, Action (..) )



{-|-}
type Item l t
           
    = Info String | Err String

    | Assume ( Ambiguation ( Cogroup t ) )
    | Body Name Flow

    | T Text
    | P Picture
    | U Url
    | Y Youtube
    | V Vimeo

{-|
`⍚`—Meta

`⌻`—Inside

`⍰`—Ambi

`⌺`—Stacked

`⌸`—Sectioning

`⍞`—Heading

`¶`—Paragraph

`⌹`—Figure

`⌯`—Figcaption

`◆`—Symbolic

`⌧`—Inaccessible
-} 
type Flow
    = Meta -- can be above, below, introducing, or trailing.
    | Inside
    | Ambi
    | Stacked
    | Sectioning
    | Heading
    | Paragraph
    | Figure
    | Figcaption
    | Symbolic
    | Inaccessible -- for bodies that are not (yet) realized.

serialize_flow : Flow -> String
serialize_flow f =
    case f of
        Meta -> "⍚"
        Inside -> "⌻"
        Ambi -> "⍰"
        Stacked -> "⌺"
        Sectioning -> "⌸"
        Heading -> "⍞"
        Paragraph -> "¶"
        Figure -> "⌹"
        Figcaption -> "⌯"
        Symbolic -> "◆"
        Inaccessible -> "⌧"
               
deserialize_flow : String -> Maybe Flow
deserialize_flow s =
    case s of
        "⍚" -> Just Meta
        "⌻" -> Just Inside
        "⍰" -> Just Ambi
        "⌺" -> Just Stacked
        "⌸" -> Just Sectioning
        "⍞" -> Just Heading
        "¶" -> Just Paragraph
        "⌹" -> Just Figure
        "⌯" -> Just Figcaption
        "◆" -> Just Symbolic
        "⌧" -> Just Inaccessible
        _ -> Nothing


                   
type alias Name = String
type alias Frozen = Data
type alias Fluid = Data

    
type Text
    -- while you edit text via a contenteditable span, the vDOM retains the frozen string\
    -- to keep the state within the Browser.
    = Text Fluid Frozen
      
type Picture
    = Picture Data

type Url
    = Url Data
      
type Youtube
    = Youtube Data
      
type Vimeo
    = Vimeo Data


type Cogroup ref
    = Referral ( Considering ref )
    | Self
      
type Considering ref
    = Open ( Nonempty ref )
    | Insatiable ref



      
-- create

{-| creates a generic symbol to represent choices where the intended group has no symbol.-}
default_symbol : Item l t
default_symbol = Body "*" Symbolic


primer : Item l t
primer = Err "primer"


initial : Item l t
initial = Info "root"

          
{-|-}     
accept : Item t t -> Item l t
accept itm =
    case itm of
        Info str -> Info str
        Err str -> Err str
        Assume ( ambiguation ) -> Assume ( ambiguation )
        Body n f -> Body n f
        T text -> T text
        P picture -> P picture
        U url -> U url
        Y youtube -> Y youtube
        V vimeo -> V vimeo

          
         
-- read


{-|-}
data : Item l t -> Maybe Data
data i =
    case i of
        T ( Text fluid frozen ) -> Just fluid
        P ( Picture dat ) -> Just dat
        U ( Url dat ) -> Just dat
        Y ( Youtube dat ) -> Just dat
        V ( Vimeo dat ) -> Just dat
        _ -> Nothing


{-| shows the name of the symbol if flow is 'Symbolic'.-}
to_symbol : Item l t -> Maybe ( Item l t )
to_symbol itm =
    case itm of
        Body n f -> if f == Symbolic then Just itm else Nothing
        _ -> Nothing

{-|-}
is_self_assumption : Item l t -> Bool
is_self_assumption itm =
    itm == Assume ( Of Self )

        
{-|-}
alternatives : Item l t -> List ( Cogroup t )
alternatives itm =
    case itm of
        Assume a ->
            ambiguation_to_list a
        _ -> []


{-|-}
assumptions : t -> Cogroup t -> List t
assumptions prototype cog =
    case cog of
        Self ->
            [ prototype ]
        Referral ( Open ( ref, refs ) ) ->
            ref::refs
        Referral ( Insatiable ref ) ->
            [ ref ]
             
             
verbalize : ( Item l t ) -> String
verbalize i =
    case i of
        Body n _ -> n
        _ -> "?"
        

{-|-}
equal : Item l t -> Item t t -> Bool
equal live_itm temp_itm =
    live_itm == accept temp_itm        
        
             
       
-- set


{-|-}
set_data : Data -> Map ( Item l t )
set_data dat i =
    case i of
        T ( Text fluid frozen ) -> T ( Text dat frozen )
        P _ -> P ( Picture dat )
        Y _ -> Y ( Youtube dat )
        V _ -> V ( Vimeo dat )
        U _ -> U ( Url dat )
        x -> x

{-|-}
freeze : Map ( Item l t )
freeze i =
    case i of
        T ( Text fluid frozen ) -> T ( Text fluid fluid )
        x -> x

{-|-}
unfreeze : Map ( Item l t )
unfreeze i =
    case i of
        T ( Text fluid frozen ) -> T ( Text frozen frozen )
        x -> x
                                   


        


-- present


-- functions not to expose
add_view =
    { proxy =
          view
          
    , flow = \n f->
          let
              insert_class =
                  View.add_class <|
                      serialize_flow f
              set_element =
                  View.element <| always <|
                      case f of
                          Symbolic -> Html.label
                          Inside -> Html.span
                          Paragraph -> Html.p
                          Sectioning -> Html.section
                          Heading -> Html.h1
                          Figure -> Html.figure
                          Figcaption -> Html.figcaption
                          _ -> Html.button
              set_text =
                  case f of
                      Symbolic -> View.set_text n
                      _ -> identity
                           
          in insert_class >> set_element >> set_text
            
    }



{-|-}
view_cogroup :
    t ->
    ( t -> View msg l t Data ) ->
    Cogroup t ->
        Map (View msg l t Data )
view_cogroup prototype make_child cog =
    let
        connect references child_views =
            View.children ( (++) child_views )
                >> View.add_action ( Choose_these references )
    in
        case cog of
           Referral ( Open ( ref, refs ) ) ->
               ( ref::refs ) |> List.map ( make_child >> View.add_class "Referral" )
                             |> connect ( ref::refs )

           Referral ( Insatiable ref ) ->
               [ ref |> make_child >> View.add_class "Insatiable" ]
                   |> connect [ ref ]
                    
           Self ->
               [ prototype |> make_child >> View.add_class "Self" ]
                   |> connect [ prototype ]
                
{-|-}
view :
    Item l t ->
        Map ( View msg l t Data )
view itm =
    case itm of

        Assume ( Of ( Self ) ) ->
            View.add_class "Assume"
                >> View.add_class "self"

        Assume ( Of ( Referral ( Insatiable ref ) ) ) ->
            View.add_class "Assume"
                >> View.add_class "insatiable"
                >> View.set_text "Insatiable."

        Assume ( Of ( Referral ( Open _ ) ) ) ->
            view ( Err "open referral in this assumption" )

        Assume ( Or head more ) ->
            View.add_class "Assume"
                >> View.add_class "options"
                >> View.set_text "..."

        Body name flow ->
            View.add_class name
                >> View.add_class "Body"
                >> View.add_class name
                >> add_view.flow name flow
                                            

        Info string ->
            View.add_class "info"
                >> View.set_text string

        T ( Text fluid frozen ) -> -- this is only the inner part, not the span around!
            View.add_class "input"
                >> View.add_class "T"

                >> View.add_action ( Targeted Contenteditable )
                >> View.add_action ( Targeted ( Input_span frozen ) )
                >> View.add_action ( Targeted Blur_span )

                -- view frozen span text to cope with contenteditable:
                >> case frozen of
                       Nothing -> identity
                       Just frozen_string ->
                           View.set_text frozen_string
                       
        P ( Picture source ) ->
            View.add_class "picture"
                >> View.add_class "P"
                >> View.element ( always Html.img )
                >> View.add_attribute ( Attributes.src ( Data.enstring source ) )

        U url ->
            View.add_class "url"
                >> View.add_class "U"
                >> View.element ( always Html.input )
                        
        Err string ->
            View.add_class string
                >> View.add_class "Err"
                
        _ -> 
            View.add_class "Todo"


{-|-}
serialize : ( l -> String ) -> ( t -> String ) -> Item l t -> String
serialize from_l from_t itm =
    let
        serialize_cogroup cog =
            case cog of
                Self ->
                    ""

                Referral ( Insatiable t ) ->
                    "∞ "++( from_t t )

                Referral ( Open ( t, tt ) ) ->
                    ( t::tt )
                        |> List.map from_t |> String.join ", " |> String.cons ' ' 

    in
        case itm of
            Assume a ->
                "<" ++ ( ambiguation_to_list a |> List.map serialize_cogroup |> String.join " |" ) ++ "\t "
        
            Body name flow ->
                name ++ "\t" ++ serialize_flow flow
                        
            Info s -> "🛈 " ++ s ++ "\t " 
            Err s -> "⚠ " ++ s ++ "\t "
            T ( Text tx t ) -> "T " ++ Data.serialize tx ++ " | " ++ Data.serialize t ++ "\t "
            P ( Picture picture ) -> "P " ++ Data.serialize picture ++ "\t "
            U ( Url url ) -> "U " ++ Data.serialize url ++ "\t "
            Y ( Youtube youtube ) -> "Y " ++ Data.serialize youtube ++ "\t "
            V ( Vimeo vimeo ) -> "V " ++ Data.serialize vimeo ++ "\t "

          
{-|-}
deserialize :
    ( String -> Maybe l ) ->
    ( String -> Maybe t ) ->
    String ->
        Item l t
deserialize to_l to_t str =
    let
        deserialize_cogroup s =
            if s == "" then
                Just Self

            else if s |> String.startsWith " ∞ " then
                     case ( s |> String.dropLeft 3 |> to_t ) of
                    Just t ->
                        t |> Insatiable >> Referral >> Just 
                    _ ->
                        Nothing

            else
                case s |> String.split ", " |> List.map to_t |> fold_must of
                    Just ts ->
                        ts |> to_nonempty
                           |> Maybe.map ( Open >> Referral )
                    _ ->
                        Nothing

    in
        case String.words str
            |> both
               ( List.head >> Maybe.withDefault ""
               , List.tail >> Maybe.withDefault [""] >> String.join " " )
        of
            ( "<", rest ) ->
                rest |> String.split " | "
                     |> List.map ( deserialize_cogroup >> Maybe.withDefault Self )
                     |> to_nonempty |> Maybe.withDefault ( Self, [] )
                     |> ambiguation_from_nonempty
                     |> Assume
                       
            ( "🛈", rest ) ->
                Info rest
            ( "⚠", rest ) ->
                Err rest
            ( "T", rest ) ->
                rest |> String.split " | "
                     |> list_to_tuple
                     |> Maybe.map ( each Data.deserialize >> \( te, xt ) -> T ( Text te xt ) ) 
                     |> Maybe.withDefault
                        ( "Failed to compose a T Text item from " ++ rest |> Err )
            ( "P", dat ) ->
                Data.deserialize dat |> Picture >> P
            ( "U", dat ) ->
                Data.deserialize dat |> Url >> U
            ( "Y", dat ) ->
                Data.deserialize dat |> Youtube >> Y
            ( "V", dat ) ->
                Data.deserialize dat |> Vimeo >> V
            _ ->
                case String.split "\t" str
                    |> List.reverse
                    |> both
                       ( List.head >> Maybe.andThen deserialize_flow
                       , List.tail >> Maybe.map ( List.reverse >> String.join "" )
                       )
                of
                    ( Just flow, Just name ) ->
                        Body name flow
                    _ -> "Unable to deserialize " ++ str ++ " to item" |> Err


module Compositron.Flow exposing
    ( Flow
    , symbolic
    , data, is_symbolic
    , set_data, freeze, unfreeze
    , serialize, deserialize
    , view )


{-|
# Definition
@docs Flow

# Create
@docs symbolic

# Read
@docs data
@docs is_symbolic

# Map
@docs set_data
@docs freeze
@docs unfreeze

# Serial Form
@docs serialize
@docs deserialize

# View
@docs view
-}
import Html exposing ( Html )
import Html.Attributes as Attributes

import Helpers exposing (..)
import Maybe.Extra
import Compositron.Data as Data exposing ( Data )

import Compositron.View as View exposing ( View, Action (..) )


{-| Directs how the browser renders an `Item`.
 
### User interface
- _Info_: A friendly, dismissible Infobox.
  # 🛈
- _Meta_: Attach to the end of a targeted item. Only render inside the targeted element.
  # ⍚
- _Symbolic_: Singular; related to the enclosing item.
   Only used to represent a _Cogroup_, else discarded.
  # ◆
- _Inaccessible_: no intended for the user interface.
  # ⌧

### Semantics
- _Inside_: Unaltered `span` item.
  # ⌻
- _Sectioning_: `section` item.
  # ⌸
- _Heading_: `h1`; related to the following contents.
  # ⍞
- _Paragraph_: `p`.
  # ¶
- _Figure_: Autonomous content. `figure`. The last _Caption_ within a figure will be a `figcaption`.
  # ❧
- _Caption_: Singular; meta or augmenting. Related to the enclosing item.
  # ℭ
- `figcaption` or `aside`, depending on the container type.
  # ⌇

### Media
- _T_ `text`: Editable content. Use in situations such as editing visible text, urls, etc.
- _P_ `picture`.
- _U_ `url`.
- _Y_ `link`: Youtube video.
- _V_ `link`: Vimeo video.

-}
type Flow
    = Meta
    | Symbolic
    | Inaccessible
    | Info String
    | Inside
    | Sectioning
    | Heading
    | Paragraph
    | Figure
    | Caption
    | T { fluid : Data, frozen : Data }
    | P Data
    | U Data
    | Y Data
    | V Data


-- create

{-|-}
symbolic: Flow
symbolic = Symbolic
      

-- read


{-|-}
data : Flow -> Maybe Data
data flow =
    case flow of
        T txt -> Just txt.fluid
        P dat -> Just dat
        U dat -> Just dat
        Y dat -> Just dat
        V dat -> Just dat
        _ -> Nothing

{-|-}
is_symbolic : Flow -> Bool
is_symbolic flow =
    flow == Symbolic



-- map


{-|-}
set_data : Data -> Map Flow
set_data dat flow =
    case flow of
        T txt -> T { txt | fluid = dat }
        P _ -> P dat
        U _ -> U dat
        Y _ -> Y dat
        V _ -> V dat
        x -> x

{-|-}
freeze : Map Flow
freeze flow =
    case flow of
        T txt -> T { txt | frozen = txt.fluid }
        x -> x
{-|-}
unfreeze : Map Flow
unfreeze flow =
    case flow of
        T txt -> T { txt | frozen = txt.fluid }
        x -> x




-- serial form



{-|-}
serialize : Flow -> String
serialize f =
    case f of
        Meta -> "⍚"
        Inside -> "⌻"
        Sectioning -> "⌸"
        Heading -> "⍞"
        Paragraph -> "¶"
        Figure -> "⌹"
        Caption -> "⌯"
        Symbolic -> "◆"
        Inaccessible -> "⌧"
        Info s -> "🛈 " ++ s
        T txt -> "T " ++ Data.serialize txt.fluid ++ " | " ++ Data.serialize txt.frozen
        P picture -> "P " ++ Data.serialize picture
        U url -> "U " ++ Data.serialize url
        Y youtube -> "Y " ++ Data.serialize youtube
        V vimeo -> "V " ++ Data.serialize vimeo

                   
{-|-}
deserialize : String -> Maybe Flow
deserialize s =
    case s of
        "⍚" -> Just Meta
        "⌻" -> Just Inside
        "⌸" -> Just Sectioning
        "⍞" -> Just Heading
        "¶" -> Just Paragraph
        "⌹" -> Just Figure
        "⌯" -> Just Caption
        "◆" -> Just Symbolic
        "⌧" -> Just Inaccessible
        other ->
            case String.split " " other
                     |> both
                        ( List.head >> Maybe.withDefault ""
                        , List.tail >> Maybe.withDefault [""] >> String.join " " )
            of
                ( "🛈", rest ) ->
                    Just ( Info rest )
                ( "T", rest ) ->
                    rest |> String.split " | "
                         |> list_to_tuple
                         |> Maybe.map
                            ( each Data.deserialize >> \( te, xt ) -> T { fluid = te, frozen =  xt } )
                         |> Maybe.Extra.or
                            ( Just <| Info ( "Failed to compose a T Text item from " ++ rest ) )
                ( "P", dat ) ->
                    Data.deserialize dat |> P >> Just
                ( "U", dat ) ->
                    Data.deserialize dat |> U >> Just
                ( "Y", dat ) ->
                    Data.deserialize dat |> Y >> Just
                ( "V", dat ) ->
                    Data.deserialize dat |> V >> Just
                _ -> Nothing



-- view


{-|-}
view : Flow -> Map ( View msg l t Data )
view flow =
    case flow of
        Symbolic ->
            View.set_element Html.label
        Inside ->
            View.set_element Html.span
        Meta ->
            View.set_element Html.span
        Inaccessible ->
            View.set_element Html.pre
        Paragraph ->
            View.set_element Html.p
        Sectioning ->
            View.set_element Html.section
        Heading ->
            View.set_element Html.h1
        Figure ->
            View.set_element Html.figure
        Caption ->
            View.set_element Html.figcaption
        Info string ->
            View.set_element
                (\att ch -> Html.div [] [ Html.h1 [] [ Html.text string ], Html.p att ch ] )
                >>View.add_class "info"
                >> View.set_text string
        T txt -> -- this is only the inner part, not the span!
            View.add_class "input"
                >> View.add_action ( Targeted Contenteditable )
                >> View.add_action ( Targeted ( Input_span txt.frozen ) )
                >> View.add_action ( Targeted Blur_span )
                -- view frozen span text to cope with contenteditable:
                >> case txt.frozen of
                       Nothing ->
                           identity
                       Just frozen_string ->
                           View.set_text frozen_string
        P source ->
            View.add_class "picture"
                >> View.element ( always Html.img )
        U url ->
            View.add_class "url"
                >> View.element ( always Html.input )
        Y _ -> identity
        V _ -> identity

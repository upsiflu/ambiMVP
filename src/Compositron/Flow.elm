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

import Compositron.View as View exposing ( View, Action (..), Role )


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

### Semantic flow
- _Page_: will try to fill the available screen.
  # 🗔
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
- _D_: Data renderer without kids.
-}
type Flow
    = Meta
    | Symbolic
    | Inaccessible
    | Page
    | Inside
    | Sectioning
    | Heading
    | Paragraph
    | Figure
    | Caption
    | D Data


-- create

{-|-}
symbolic: Flow
symbolic = Symbolic
      

-- read


{-|-}
data : Flow -> Maybe Data
data flow =
    case flow of
        D d -> Just d
        _ -> Nothing

{-|-}
is_symbolic : Flow -> Bool
is_symbolic flow =
    flow == Symbolic || flow == Meta



-- map


map_data fu flow =
    case flow of
        D dat -> D ( fu dat )
        _ -> flow

{-|-}
set_data : Data -> Map Flow
set_data dat =
    map_data ( \_-> dat )

{-|-}
freeze : Map Flow
freeze =
    map_data Data.freeze
    
{-|-}
unfreeze : Map Flow
unfreeze =
    map_data Data.unfreeze




-- serial form



{-|-}
serialize : Flow -> String
serialize f =
    case f of
        Meta -> "⍚"
        Page -> "🗔"
        Inside -> "⌻"
        Sectioning -> "⌸"
        Heading -> "⍞"
        Paragraph -> "¶"
        Figure -> "⌹"
        Caption -> "⌯"
        Symbolic -> "◆"
        Inaccessible -> "⌧"
        D dat -> Data.serialize dat 

                   
{-|-}
deserialize : String -> Maybe Flow
deserialize s =
    case s of
        "⍚" -> Just Meta
        "🗔" -> Just Page
        "⌻" -> Just Inside
        "⌸" -> Just Sectioning
        "⍞" -> Just Heading
        "¶" -> Just Paragraph
        "⌹" -> Just Figure
        "⌯" -> Just Caption
        "◆" -> Just Symbolic
        "⌧" -> Just Inaccessible
        other ->
            Data.deserialize other
                |> Maybe.map D



-- view


{-|-}
view : Flow -> Map ( View node t )
view flow =
    case flow of
        Symbolic ->
            identity
        Page ->
            View.role View.P
        Inside ->
            View.role View.Span
        Meta ->
            identity
        Inaccessible ->
            identity
        Paragraph ->
            View.role View.P
        Sectioning ->
            View.role View.P
        Heading ->
            View.role View.H
        Figure ->
            View.role View.F
        Caption ->
            View.role View.C
        D d ->
            View.decode d
                >> View.action ( Class <| Data.serialize_constructor d )
                >> View.action ( When_targeted Contenteditable )
                >> View.action ( When_targeted Blur_span )
                >> View.action ( When_targeted ( Input_span d ) )
   

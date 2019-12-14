module Compositron.Role exposing
    ( Role (..)
    , is_symbolic
    , serialize, deserialize
    )


{-|
# Definition
@docs Role

# Read
@docs is_symbolic

# Serial Form
@docs serialize
@docs deserialize
-}

import Html exposing ( Html )
import Html.Attributes as Attributes

import Helpers exposing (..)


{-| Directs how the browser renders a `Body`.
 
While a structure of nested Groups comprises _inherent_ relations, we use flow tags
to add interface, meaning, or data to them.

Contrary to < and >, which channel shape resp. data from one group to another,
flow tags never actually affect the flow of data or structure through the composition.
Instead, they help translate the composition to a user interface in the medium of Html.

### Semantic tags

- # ▓
  _Page_: Exclusive container; will try to fill the available screen.
- # ℌ
  _Heading_: Outline Item; `h1`; related to the following contents.
- # ¶ 
  _Paragraph_: Sectioning container; similar to `p`, but more permissive in content type.
- # ❦
  _Figure_: Autonomous content; `figure`. The last _Caption_ within a figure will be a `figcaption`.
- # ℭ
  _Caption_: A commentary on (or description of) the surroundings.
- # ⌇
  _Aside_: Secondary content; `aside`.
- # ⟷
  _Span_: Grouping of a series of items; `span`.


### User Interface Flow tags

Bodies tagged with a User Interface flow enrich the actual composition with one-to-one and one-to-many relations.
As these items will take no space in the layout, but need to be accessible in order to satisfy the tangibility tenet, they will appear in clumps and menus.

- # ⍛ 
  _Tag_: Tag a group as a whole. Use for _facet classification_ such as style classes, hashtags, and other explicit categorization tasks. Data will be shared per group.
  You can escalate tags: ⍛⍛ lifts data to the parent's parent, and so on. Assumptions 'tagify' the head of the template branches, and nesting the _assume_ operator `< <` will escalate the tag-icity.
- # 🜺 
  _Representation_: Stands in for a group as long as it is eventual, i.e. a _Cogroup_.
  The body will appear as a _face_ in an option. Once the cogroup is instanciated, you find the representation clumped so to be tangible directly.
- # ⮾
  _Nopresentation_: remove the item (but not its deascendants) from the user interface. Will appear in a clump so you can edit it.
  

### Semantic Tags (see [`View`](Compositron.View)!)

These tags provide a semantic metastructure to the bodies in a tree.
They are 'built-in' classes or tags, closely related to Html tags.

-}
type Role
    = Tag
    | Nopresentation
    | Page
    | Heading
    | Paragraph
    | Figure
    | Caption
    | Aside
    | Span



-- read


{-|-}
is_symbolic : Role -> Bool
is_symbolic r =
    r == Nopresentation || r == Tag




-- serial form


{-|-}
serialize : Role -> String
serialize r =
    case r of
        Tag ->
            "⍛"
        Nopresentation ->
            "⮾"
        Page ->
            "▓"
        Heading ->
            "ℌ"
        Paragraph ->
            "¶"
        Figure ->
            "❦"
        Caption ->
            "ℭ"
        Aside ->
            "⌇"
        Span ->
            "⟷"
                  
{-|-}
deserialize : String -> Maybe Role
deserialize s =
    case s of
        "⍛" ->
            Just Tag 
        "⮾" ->
            Just Nopresentation 
        "▓" ->
            Just Page 
        "ℌ" ->
            Just Heading
        "¶" ->
            Just Paragraph
        "❦" ->
            Just Figure
        "ℭ" ->
            Just Caption
        "⌇" ->
            Just Aside
        "⟷" ->
            Just Span
        _ ->
            Nothing

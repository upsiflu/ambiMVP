module Compositron.Flow exposing
    ( Flow
    , symbolic
    , data, is_symbolic
    , set_data, freeze, unfreeze
    , serialize, deserialize
    , view )


{-|
Flow tags annotate bodies to help translate a composition to the medium of Html.
As all other element ofs a composition are medium-agnostic, flow tags must enumerate
all features that we want an Html/css rendition to expose, namely:

- **user interface concerns** such as info boxes, tagging, group representation, and
accessibility,
- **semantic markup** such as outlining, labelling and sectioning, and finally
- **data generators** that manage Html-specific data types (see [`Data`](Data#Data)). 

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

import Compositron.View as View exposing ( View, Action (..), Role (..) )


{-| Directs how the browser renders a `Body`.
 
While a structure of nested Groups comprises _inherent_ relations, we use flow tags
to add interface, meaning, or data to them.

Contrary to < and >, which channel shape resp. data from one group to another,
flow tags never actually affect the flow of data or structure through the composition.
Instead, they help translate the composition to a user interface in the medium of Html.


### User Interface Flows

Bodies tagged with a User Interface flow enrich the actual composition with one-to-one and one-to-many relations.
As these items will take no space in the layout, but need to be accessible in order to satisfy the tangibility tenet, they will appear in clumps and menus.

- # ⍛ 
  _Tag_: Tag a group. Use for _facet classification_ such as style classes, hashtags, and other explicit categorization tasks.
- # 🜺 
  _Representation_: Stands in for a group as long as it is eventual, i.e. a _Cogroup_.
  The body will appear as a _face_ in an option. Once the cogroup is instanciated, you find the representation clumped so to be tangible directly.
- # ⮾
  _Nopresentation_: remove the item (but not its deascendants) from the user interface. Will appear in a clump so you can edit it.
  

### Semantic Tags (see [`View`](Compositron.View)!)

These tags provide a semantic metastructure to the bodies in a tree.
They are 'built-in' classes or tags, closely related to Html tags.

### Data Extension

- _D_: Data renderer without kids.
-}

type Flow
    = I Interfacing
    | S View.Role
    | D Data

type Interfacing
    = Tag
    | Representation
    | Nopresentation      
      
-- create

{-|-}
symbolic: Flow
symbolic = I Representation
      

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
    flow == I Representation || flow == I Tag



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
        I Tag ->
            "⍛"
        I Representation ->
            "🜺"
        I Nopresentation ->
            "⮾"
        S rl ->
            View.serialize_role rl
        D dat ->
            Data.serialize dat 

                   
{-|-}
deserialize : String -> Maybe Flow
deserialize s =
    case s of
        "⍛" ->
            Just ( I Tag )
        "🜺" ->
            Just ( I Representation )
        "⮾" ->
            Just ( I Nopresentation )
        other ->
            case other |> both ( View.deserialize_role, Data.deserialize ) of
                ( Just r, _ ) ->
                    Just ( S r )
                ( _, Just d ) ->
                    Just ( D d )
                _ ->
                    Nothing


-- view


{-|-}
view : Flow -> Map ( View node t )
view flow =
    case flow of
        I Tag ->
            identity
        I Representation ->
            identity
        I Nopresentation ->
            identity
        S role ->
            View.role role
        D d ->
            View.decode d
                >> View.action ( Class <| Data.serialize_constructor d )
                >> View.action ( When_targeted Contenteditable )
                >> View.action ( When_targeted Blur_span )
                >> View.action ( When_targeted ( Input_span d ) )
   

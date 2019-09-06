module Compositron.View exposing
    ( View

    , live
    , err

    , face
    , kid
    , kids
    , target
    , decode
    , Action (..)
    , action
    , option
    , options
    , Role (..)
    , role

    , view
    )

{-| Each View corresponds to one Live Branch in a Compositron.
The relation to Html is more complex as three rules guide the
adaptation of an arbitrary COmpositron into semantically correct
Html5.

@docs View

## Relation of View Constructors with Nodes and Html

A _Transient_ node, originating at an ambiguous Assume, has a gatekeeper function.
Its 'kids' are Options, originating at a Template.
In Html, the option menu is only drawn when the Transient is Targeted.
      
Self Assumptions and Symbolics produce _Tags_ relating to their container.
    
The _Container_ originates from a Body with kids.
In Html, it may require wrapping:

🅁 𝐻 may only have 'phrasing' content. So  map  kids.

Additionally, a container responds to the size attribute of
their kids and may need to clump groups of too_small kids:

🅁 If successive items are too small, then clump them in groups that
expand when any member or descendent thereof is open.
    
Renderer: a body with data-bearing flow.
Becomes a Html element with source or seed attributes, such as `text`, `a` or `img`.
Children become siblings (or labels?) in the Html tree.

🅁 [contenteditable] can't have children. --> Wrap in an outer span.

_Error_: an Error node.
Ignore children and just display a collapsible error message in place.

# Create
@docs live

# Map
@docs err
@docs target
@docs face
@docs decode

## Kids
@docs kid
@docs kids

## Action
@docs Action
@docs action

## Option
@docs option
@docs options

## Role
@docs Role
@docs role

# View
@docs view
-}


import Helpers exposing (..)

import Html exposing (..)
import Html.Attributes as A exposing (property)
import Compositron.Data as Data exposing ( Data (..) )

import Html.Events as Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)


{-|-}
type View node prototype
    = Transient
      ( Parameters node ) ( Nonempty ( Option prototype ) )
    | Tag
      ( Parameters node )
    | Renderer
      ( Parameters node ) Data
    | Container
      ( Parameters node ) Role ( List ( Kid node prototype ) )
    | Error
      ( Parameters node ) String

type alias Option prototype =
    List prototype
        
{-|-}
type Action
    = Id String
    | Class String
    | Navigate_here
    | Focus_here
    | Input_url Data
    | Input_span Data
    | Blur_span
    | Contenteditable
    | When_targeted Action
    
type alias Parameters node =
    { node : node
    , open : Bool
    , face : String
    , actions : List Action
    }


    
-- create


{-|-}      
live : node -> View node prototype
live n = Tag
    { node = n
    , open = False
    , face = "***"
    , actions = []
    }



parameters v =
    case v of
        Transient p os -> p 
        Tag p -> p
        Renderer p d -> p
        Container p r ks -> p
        Error p s -> p
        
map_parameters : Map ( Parameters node ) -> Map ( View node prototype )
map_parameters fu v =
    case v of
        Transient p os ->
            Transient ( fu p ) os 
        Tag p ->
            Tag ( fu p )
        Renderer p d ->
            Renderer ( fu p ) d
        Container p r ks ->
            Container ( fu p ) r ks
        Error p s ->
            Error ( fu p ) s


{-|-}
err : String -> Map ( View node prototype )
err str v = Error ( parameters v ) str

{-|-}
target : Map ( View node prototype )
target =
    map_parameters ( \p -> { p | open = True } )
        >> action ( Class "t" )

{-|-}
type Role
    = P 
    | H
    | F
    | C
    | Span
                   
{-|-}
role : Role -> Map ( View node prototype )
role rl v =
    case v of
        Container p r ks ->
            Container p rl ks
        _ ->
            Container ( parameters v ) rl []
                
{-|-}
decode : Data -> Map ( View node prototype )
decode d v =
    Renderer ( parameters v ) d
        
{-|-}
face : String -> Map ( View node prototype )
face fa v =
    v |> map_parameters
        ( \p -> { p | face = fa++"/"++p.face } )

{-|-}    
action : Action -> Map ( View node prototype )
action ac v =
    v |> map_parameters
        ( \p -> { p | actions = ac::p.actions } )


{-|-}            
kids : List ( View node prototype ) -> Map ( View node prototype )
kids vs =
    case vs of
        [] -> identity
        [vw] -> kid vw
        vw::vww -> kid vw >> kids vww
        
{-|-}            
kid : View node prototype -> Map ( View node prototype )
kid new v =
    let
        size : View node prototype -> Int
        size viw =
            case viw of
                Renderer p d ->
                    Data.size d
                     
                Container p Span ks ->
                    ks |> List.foldl
                          ( \k acc ->
                                (+) acc <|
                                case k of
                                    Kid vw   ->
                                        size vw
                                    Clump ( cl, ump ) ->
                                        cl::ump |> List.map size
                                                |> List.foldl (+) 0
                          ) 0
                Container p _ ks -> 10
                _ -> 0

        measure = if size new > 4 then Sufficient else TooSmall
    in
        case v of
            Container p r [] ->
                case measure of
                    TooSmall ->
                        Container p r [ Clump ( new, [] ) ]

                    Sufficient ->
                        Container p r [ Kid new ]
                    
            Container p r ( k::kk ) ->
                case ( measure, k ) of
                    ( TooSmall, Clump ( cl, umped ) ) ->
                        Container p r ( Clump ( new, cl::umped )::kk )
                            
                    ( TooSmall, Kid _ ) ->
                        Container p r ( Clump ( new, [] )::k::kk )
                            
                    ( Sufficient, _ ) ->
                        Container p r ( Kid new::k::kk )
                            
            _ -> v

{-|-}
options : List ( Option prototype ) -> Map ( View node prototype )
options os =
    case os of
        [] -> identity
        [op] -> option op
        op::tions -> option op >> options tions
                 
{-|-}                 
option : Option prototype -> Map ( View node prototype )
option op v =
    case v of
        Transient p ( t, ions ) ->
            Transient p ( op, t::ions )
        Tag p ->
            Transient p ( op, [] )
        _ ->
            err "Error in View.option" v
            
    
type Measure
    = Sufficient
    | TooSmall
             
type Kid node prototype
    = Kid ( View node prototype )
    | Clump ( Nonempty ( View node prototype ) )

           




-- view


{-|-}
view :
    { to_attribute : node -> Action -> Attribute msg
    , to_choice : node -> Option prototype -> { choice : Attribute msg, face : String }
    }
    -> View node prototype
    -> Html msg
view context v =
    let
        this = parameters v |> .node
        to_attributes = this
            |> context.to_attribute
            |> List.map
        to_choice = this
            |> context.to_choice

        wrapper =
            case v of
                Renderer p d ->
                    \el -> span [ A.class "wrapper" ] [ el, label [ A.class "Tag face" ] [ text p.face ] ]
                _ ->
                    identity
                        
        element =
            case v of
                Transient p os ->
                    button
                Tag p ->
                    label
                Renderer p ( Text t ) ->
                    Html.node "editable-span"
                Renderer p d ->
                    span
                Container p P ks ->
                    div
                Container p H ks ->
                    h1
                Container p F ks ->
                    figure
                Container p C ks ->
                    div
                Container p Span ks ->
                    span
                Error p s ->
                    div

        role_to_attribute r =
            case r of
                P -> "¶"
                H -> "ℌ"
                F -> "❦"
                C -> "ℭ"
                Span -> "⟷"
                
        attributes = 
            case v of
                Transient p os ->
                    to_attributes p.actions
                        |> (::) ( A.class "◇" )
                Tag p ->
                    to_attributes p.actions
                        |> (::) ( A.class "◆" )
                Renderer p ( Text t ) ->
                    to_attributes p.actions
                        |> (::) ( t.frozen |> Maybe.withDefault "" |> Encode.string >> property "string" )
                Renderer p d ->
                    to_attributes p.actions
                        |> (::) ( A.class "C" )
                        |> (::) ( A.contenteditable False )
                Container p r ks ->
                    to_attributes p.actions
                        |> (::) ( A.class "C" )
                        |> (::) ( A.class ( role_to_attribute r ) )
                Error p s ->
                    [ A.class "Error" ]

        children =
            case v of
                Transient p ( op, tions ) ->
                    [ label [ A.class "face pl" ] [ text "+" ]
                    , span [ A.class "connector-v" ] []
                    , ul [] ( List.map ( to_choice >> deopt ) ( op::tions ) )
                    ]
                Tag p ->
                    [ text p.face ]
                Renderer p d ->
                    Data.view d
                Container p r ks ->
                    List.map dekid ( List.reverse ks )
                        |> (::) ( label [ A.class "Tag face" ] [ text p.face ] )
                Error par s ->
                    [ h3 [] [ text "Error" ], p [] [ text s ] ]

        deface str =
            span [ A.class "face" ] [ text str ]
                        
        deopt o =
            li [] [ button [ A.class "Option", o.choice ] [ deface o.face ] ]
            
        dekid k =
          case k of
              Kid vw ->
                  view context vw
              Clump ( cl, ump ) ->
                  view_clump_menu ( cl::ump )

        view_clump_menu : List ( View node prototype ) -> Html msg
        view_clump_menu vv =
            let
                all_kids vw =
                    case vw of
                        Container p r ks ->
                            ks
                        _ -> []
                view_is_open vw =
                    parameters vw
                        |> .open
                        |> (||) ( all_kids vw |> List.any kid_is_open )
                kid_is_open k =
                    case k of
                        Kid vw ->
                            view_is_open vw
                        Clump ( cl, ump ) ->
                            cl::ump
                                |> List.any view_is_open
                view_clumped_entry vw =
                    li [] [ view context vw ]

            in
                -- draw the menu; use css to expand and compress it.
                case ( List.any view_is_open vv, List.head vv ) of
                    ( True, _ ) ->
                        -- draw the expanded menu
                        div [ A.class "clump" ]
                            [ span [ A.class "face cl blink" ] [ text "][" ]
                            , ul [ A.class "expanded" ] ( List.map view_clumped_entry vv )
                            ]
                    ( False, Just pivot ) ->
                        -- draw the collapsed menu. Show the first clumped entry before the tag.
                        div [ A.class "clump" ]
                            [ span [ A.class "face cl blink" ] [ text "][" ]
                            , span [ A.class "collapsed" ] [ view context pivot ]
                            ]
                    ( False, Nothing ) ->
                        text ""
                    
                
    in
        element attributes children |> wrapper

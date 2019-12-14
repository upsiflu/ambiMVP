module Compositron.View exposing
    ( View

    -- Create
    , live
        
    -- Compositron
    , contain
    
    -- Item (A, B, D, E)
    , indicate
    , offer
    , play
    , generate
    , report

    -- adding interactivity????
    , Action (..)
    , action


    -- Html Form
    , view
    )

{-| Each View corresponds to one Live Branch in a Compositron.
The relation to Html is more complex as three rules guide the
adaptation of an arbitrary COmpositron into semantically correct
Html5.

@docs View

## Relation of View Constructors with Nodes and Html
      
Self Assumptions and Symbolics produce _Tags_ relating to their container.
    
The _Container_ originates from a Body with kids.
In Html, it may require wrapping.

Additionally, a container responds to the size attribute of
their kids and may need to clump groups of too_small kids:

🅁 If successive items are too small, then clump them in groups that
expand when any member or descendent thereof is open.

🅁 ℌ may only have 'phrasing' content. So  map  kids.

    
Renderer: a body with data-bearing flow.
Becomes a Html element with source or seed attributes, such as `text`, `a` or `img`.
Children become siblings (or labels?) in the Html tree.

🅁 [contenteditable] can't have children. --> Wrap in an outer span.

_Error_: an Error node.
Ignore children and just display a collapsible error message in place.

# Create
@docs live

# Map

## Data
@docs generate

## Roles
@docs play

## Choices
@docs indicate
@docs offer

## Kids
@docs contain

## Actions
@docs Action
@docs action

## Errors
@docs report

# View to Html
@docs view
-}


import Helpers exposing (..)
import Helpers.Nonempty as Nonempty exposing ( Nonempty )

import Html exposing ( Html, label, text, del, figure, figcaption, h1, div, span, button, aside, section )

import Compositron.Data as Data exposing ( Data (..) )
import Compositron.Role as Role exposing ( Role (..) )
import Compositron.Arrow as Arrow exposing ( Arrow (..) )
import Compositron.Cogroup as Cogroup exposing ( Cogroup (..) )
import Compositron.Manifestation as Manifestation exposing ( Manifestation )

import Html.Events as Events exposing (..)
import Html.Attributes exposing ( class, id, property )
import Json.Decode as Decode
import Json.Encode as Encode exposing ( Value )


{-|-}
type View node prototype
    = Placeholder
      ( Parameters node )
 -- Assumption Node
    | Indicator
      ( Parameters node ) ( Cogroup prototype )
    | Options
      ( Parameters node ) ( Nonempty ( Cogroup prototype ) )
 -- Body Node
    | Leaf
      ( Parameters node ) String Role
    | Container
      ( Parameters node ) String Role ( Nonempty ( Kid node prototype ) )
 -- Charge Node
 -- Data Node
    | Renderer
      ( Parameters node ) Data
 -- Error Node
    | Error
      ( Parameters node ) String

    
type alias Parameters node =
    { node : node
    , manifestation : Manifestation
    , actions : List Action
    }
    
{-|-}
type Action
    = Id String
    | Class String
    | Target_here
    | Tab_here
    | Focus_here
    | Input_url Data
    | Input_span Data
    | Blur_span
    | Contenteditable
    | When_targeted Action
    | Property String Value
    


    
-- create


{-|-}      
live : String -> node -> Manifestation -> View node prototype
live sig nod man =
    Placeholder
    { node = nod
    , actions = [ Id sig, Class "item" ]
    , manifestation = man
    }



-- modify

to_clump : Map ( View node prototype )
to_clump =
    parameters
      >> \p-> Placeholder { p | actions = ( Class "clump closed" )::p.actions }

-- read

kids v =
    case v of
        Container _ _ _ ( k, ks ) -> k::ks
        _ -> []
          

parameters v =
    case v of
        Placeholder p -> p
        Indicator p _ -> p
        Options p _ -> p
        Leaf p _ _ -> p
        Container p _ _ _ -> p
        Renderer p _ -> p
        Error p _ -> p
        
map_parameters : Map ( Parameters node ) -> Map ( View node prototype )
map_parameters fu v =
    case v of
        Placeholder p -> Placeholder ( fu p )
        Indicator p t -> Indicator ( fu p ) t
        Options p o -> Options ( fu p ) o
        Leaf p n r -> Leaf ( fu p ) n r
        Container p n r k -> Container ( fu p ) n r k
        Renderer p d -> Renderer ( fu p ) d
        Error p m -> Error ( fu p ) m


{-|-}
indicate : Cogroup prototype -> Map ( View node prototype )
indicate cog v =
    Indicator ( parameters v ) cog
                     
{-|-}
report : String -> Map ( View node prototype )
report message v =
    Error ( parameters v ) message

                   
{-|-}
play : String -> Role -> Map ( View node prototype )
play n r v =
    case v of
        Leaf p _ _ ->
            Leaf p n r
        Container p _ _ ks ->
            Container p n r ks
        _ ->
            Leaf ( parameters v ) n r
                
{-|-}
generate : Data -> Map ( View node prototype )
generate d v =
    Renderer ( parameters v ) d
        

{-|-}    
action : Action -> Map ( View node prototype )
action ac v =
    v |> map_parameters
        ( \p -> { p | actions = ac::p.actions } )


            
{-|-}            
contain : Nonempty ( View node prototype ) -> Map ( View node prototype )
contain n v =
    Nonempty.fold kid v n 
        
{-|-}
offer : Nonempty ( Cogroup prototype ) -> Map ( View node prototype )
offer n v =
    Nonempty.fold option v n 
                           
option : Cogroup prototype -> Map ( View node prototype )
option op v =
    case v of
        Options p ( o, os ) -> Options p ( op, o::os )
        _ -> Options ( parameters v ) ( op, [] )
            


kid : View node prototype -> Map ( View node prototype )
kid new v =
    let
        size : View node prototype -> Int
        size viw =
            case viw of
                Renderer p d ->
                    Data.size d
                     
                Container p _ Span ( k, ks ) ->
                    k::ks |> List.foldl
                          ( \ck acc ->
                                (+) acc <|
                                case ck of
                                    Kid vw   ->
                                        size vw
                                    Clump ( cl, ump ) ->
                                        cl::ump |> List.map size
                                                |> List.foldl (+) 0
                          ) 0
                Container p _ _ ks -> 7
                _ -> 0

        measure = if size new > 3 then Sufficient else TooSmall
    in
        case v of
            Leaf p n r ->
                case measure of
                    TooSmall ->
                        Container p n r ( Clump ( new, [] ), [] )

                    Sufficient ->
                        Container p n r ( Kid new, [] )
                    
            Container p n r ( k, kk ) ->
                case ( measure, k ) of
                    ( TooSmall, Clump ( cl, umped ) ) ->
                        Container p n r ( Clump ( new, cl::umped ), kk )
                            
                    ( TooSmall, Kid _ ) ->
                        Container p n r ( Clump ( new, [] ), k::kk )
                            
                    ( Sufficient, _ ) ->
                        Container p n r ( Kid new, k::kk )
                            
            _ -> v
                     

            
    
type Measure
    = Sufficient
    | TooSmall
             
type Kid node prototype
    = Kid ( View node prototype )
    | Clump ( Nonempty ( View node prototype ) )

           
                
-- view


{-|-}
view :
    { to_attribute : node -> Action -> Html.Attribute msg
    , to_choice : node -> Cogroup prototype -> { choice : Html.Attribute msg, face : String }
    }
    -> View node prototype
    -> Html msg
view context v =
    let
        this = parameters v |> .node

        focus_indicator = label [ class "focus" ] [ text "✢" ]
                          
        config =
            let button_labeled tag face attr ks =
                    [ case face of
                          Nothing -> text ""
                          Just txt -> label [] [ text txt ]
                    , span [ class "tag", class tag ] [ text tag ]
                    , focus_indicator
                    ] |> (++) ks
                      |> button attr
                         
                default =
                    { wrap = \ks element_with_attr ->
                          element_with_attr ( focus_indicator::ks )
                    , element = button
                    , actions = []
                    , children = []
                    }
            in
            case v of
                Placeholder p ->
                    default
                Indicator p i ->
                    { default
                    | element =
                        i |> Cogroup.arrows
                          |> List.map ( Arrow.serialize ( always "" ) )
                          |> String.join ", "
                          |> Just >> button_labeled ""
                    , actions =
                        i |> Cogroup.arrows
                          |> List.map
                             ( \j -> case j of
                                       Self -> Class "SA"
                                       Cyclic _ -> Class "CA"
                                       Irresolvable _ -> Class "IrrA"
                                       Open _ -> Class ""
                             )
                    }
                Options p ( o, os ) ->
                    { default
                    | element = button_labeled "+" Nothing
                    , actions = [ Class "M O" ]
                    , children =
                        let
                            view_button cho =
                                button [ class "option", cho.choice ]
                                       [ span [ class "label" ] [ text cho.face ] ]
                        in
                            ( o::os )
                                |> List.filter ( not << Cogroup.is_self )
                                |> List.map ( context.to_choice p.node >> view_button )
                                |> \buttons -> [ span [ class "options" ] buttons ]
                    }
                Leaf p n r ->
                    { default
                    | element = button_labeled "" ( Just n )
                    , actions = [ Class ( Role.serialize r ) ]
                    }
                Container p n r ( k, ks ) ->
                    { default
                    | element =
                        case r of
                            Tag -> span
                            Nopresentation -> del
                            Page -> section
                            Heading -> h1
                            Paragraph -> Html.p
                            Figure -> figure
                            Caption -> figcaption
                            Aside -> aside
                            Span -> span
                    , actions = [ Class ( "C " ++ Role.serialize r ) ]
                    , children =
                        let
                            view_clump_menu : Nonempty ( View node prototype ) -> Html msg
                            view_clump_menu ( cv, cvs ) =
                                let
                                    view_is_open vw =
                                        parameters vw
                                            |> .manifestation >> Manifestation.targeted
                                            |> (||) ( kids vw |> List.any kid_is_open )
                                    kid_is_open ck =
                                        case ck of
                                            Kid vw ->
                                                view_is_open vw
                                            Clump ( cl, ump ) ->
                                                cl::ump |> List.any view_is_open
                           
                                    wire vv =
                                        List.map ( view context ) vv
                                            |> div [ class "wire" ]
                                        

                                in
                                    if List.any view_is_open ( cv::cvs )
                                        then
                                            span
                                                [ class "clump opened" ]
                                                [ text "​"
                                                , div [ class "bar" ] [ wire ( cv::cvs ) ]
                                                ]
                                        else
                                            cv |> to_clump
                                               |> view context
                                            
                        in
                            k::ks
                            |> List.reverse
                            |> List.map (\ck->
                                case ck of
                                    Kid subview ->
                                        view context subview
                                    Clump clumped ->
                                        view_clump_menu clumped )
                               
                    }
                Renderer p ( Text t ) ->
                    { default
                    | element = Html.node "editable-span"
                    , actions = 
                        [ t.frozen |> Maybe.withDefault "" |> Encode.string >> Property "string"
                        , t.frozen |> Maybe.withDefault "" |> Encode.string >> Property "name" ]
                    }
                Renderer p d ->
                    { default
                    | element = span
                    , children = Data.view d
                    }
                Error p m ->
                    { default
                    | element = Html.p
                    , children = [ h1 [] [ text "Error" ], Html.p [] [ text m ] ]
                    }
        
            

                    
                
    in
        Target_here :: Tab_here :: config.actions ++ .actions ( parameters v )
            |> List.map ( context.to_attribute this )
            |> config.element
            |> config.wrap config.children

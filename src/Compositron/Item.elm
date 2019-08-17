module Compositron.Item exposing
    (..)

import Html exposing ( Html )
import Html.Attributes as Attributes

import Helpers exposing (..)

import Compositron.Data as Data exposing ( Data )
import Compositron.Signature as Signature exposing ( Signature )

import Compositron.View as View exposing ( View, Action (..) )




type Item l t
           
    = Info String | Err String

    | Assume ( Ambiguation ( Cogroup t ) )
    | Body Name Flow

    | T Text
    | P Picture
    | U Url
    | Y Youtube
    | V Vimeo

      
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
    | Inaccessible -- for bodies that are not (yet) realized.

      
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


primer : Item l t
primer = Err "primer"


initial : Item l t
initial = Info "root"

          
         

-- read


self_reference : Item l t -> Bool
self_reference itm =
    itm == Assume ( Of Self )

                
alternatives : Item l t -> List ( Cogroup t )
alternatives itm =
    let
        accumulate ambiguation =
            case ambiguation of
                
                Or cog alternative ->
                    cog :: ( accumulate alternative )
                        
                Of cog ->
                    [ cog ]
                
    in case itm of
        Assume ambiguation ->
            accumulate ambiguation
        _ -> []


assumptions : t -> Cogroup t -> List t
assumptions prototype cog =
    case cog of
        Self ->
            [ prototype ]
        Referral ( Open ( ref, refs ) ) ->
            [ ref::refs ]
        Referral ( Insatiable ref ) ->
            [ ref ]
             
             
verbalize : ( Item l t ) -> String
verbalize i =
    case i of
        Body n _ _ -> n
        _ -> "?"

data : Item l t -> Maybe Data
data i =
    case i of
        T ( Text fluid frozen ) -> Just fluid
        P ( Picture dat ) -> Just dat
        U ( Url dat ) -> Just dat
        Y ( Youtube dat ) -> Just dat
        V ( Vimeo dat ) -> Just dat
        _ -> Nothing



             
-- transformer


info : Map ( Item l t )
info = serialize >> Info


       
-- set


set_data dat i =
    case i of
        T ( Text fluid frozen ) -> T ( Text dat frozen )
        P _ -> P ( Picture dat )
        Y _ -> Y ( Youtube dat )
        V _ -> V ( Vimeo dat )
        U _ -> U ( Url dat )
        x -> x

freeze i =
    case i of
        T ( Text fluid frozen ) -> T ( Text fluid fluid )
        x -> x

unfreeze i =
    case i of
        T ( Text fluid frozen ) -> T ( Text frozen frozen )
        x -> x
                                   


        
-- macros

{--
type Macro
    = Macro_Paragraph
    | Macro_Figure
    | Macro_Text


demacro : Macro -> Item ref      
demacro m =
    case m of
        Macro_Paragraph ->
               Body "paragraph" Paragraph
                <| Of ( I <| Assume <| Self End
                      , [ I <| Body "+" Ambi
                              <| Of ( I blocks, [] )
                              <| or ( I spans, [] )
                                  End
                        ]
                      , I layout
                      )
                    End
                        
        Macro_Figure ->
               Body "figure" Figure
                <| Of ( Body "figure media" Ambi
                            <| Of ( I <| P <| Picture ( Nothing ), [] )
                            <| or ( I <| Y <| Youtube ( Nothing ), [] )
                            <| or ( I <| V <| Vimeo ( Nothing ), [] )
                            <| or ( I blocks, [] )
                                End
                      , [ Body "caption" Figcaption
                              <| Of ( I <| Body "caption" Ambi
                                          <| Of ( I blocks, [] )
                                          <| or ( I spans, [] )
                                              End
                                    , [] )
                                  End
                        , I layout
                        ]
                      )
                    End

        Macro_Text ->
               Body "text" Inside
                <| Of ( I spans
                      , [ I <| T <| Text Nothing Nothing
                        , I <| Body "+" Ambi
                            <| Of ( I link, [] )
                            <| reset
                                End
                        , I layout
                        ]
                      )
                    End


style s =
    Body s Stacked
        <| Of ( I <| Info s, [] )
            End
                
layout =
    Body "layout" Meta
        <| Of ( I <| Assume <| Self End
              , [ I <| Body "+" Ambi
                      <| Of ( I <| style "align right", [] )
                      <| or ( I <| style "align centered", [] )
                      <| or ( I <| style "shaded green", [] )
                ]
              )
            End
        
blocks =
    Body "block..." Ambi
        <| Of ( M Macro_Paragraph, [] )
        <| or ( M Macro_Figure, [] )
        <| or ( I heading, [] )
            End
        
spans =
    Body "in line..." Ambi
        <| Of ( M Macro_Text, [] )
        <| reset
            End


heading =
    Body "heading" Heading
        <| Of ( I <| Assume <| Self End
              , [ I spans, I layout ]
              )
            End
        
link t u =
    Body "link" Inside
        <| Of ( I <| Body "destination" Meta
                    <| Of ( I <| U <| Url u, [] )
                        End
              , [ I <| T <| Text t t ]
              )
            End
        
proxy itm =
    Body "proxy" Stacked
        <| Of ( I itm, [] ) End
        
--}
            
-- present


-- functions not to expose
add_view =
    { proxy =
          view
          
    , flow = \f->
          let
              insert_class =
                  View.add_class <|
                      case f of
                          Meta -> "meta"
                          Inaccessible -> "inaccessible"
                          Stacked -> "stacked"
                          _ -> ""
              set_element =
                  View.element <| always <|
                      case f of
                          Inside -> Html.span -- default
                          Paragraph -> Html.p
                          Sectioning -> Html.section
                          Heading -> Html.h1
                          Figure -> Html.figure
                          Figcaption -> Html.figcaption
                          _ -> Html.button
          in insert_class >> set_element
            
    }



view_cogroup :
    t ->
    ( t -> View msg ( Item l t ) l Data ) ->
    Cogroup t ->
        Map (View msg ( Item l t ) l Data )
view_cogroup prototype make_child cog =

    let
        connect references child_views =
            View.children ( (++) child_views )
                >> View.add_action ( Choose_these references )
            
    in case cog of
           Referral ( Open ( ref, refs ) ) ->
               ( ref::refs ) |> List.map ( make_child >> View.add_class "Referral" )
                             |> connect ( ref::refs )

           Referral ( Insatiable ref ) ->
               [ ref |> make_child >> View.add_class "Insatiable" ]
                   |> connect [ ref ]
                    
           Self ->
               [ prototype |> make_child >> View.add_class "Self" ]
                   |> connect [ prototype ]

    

    
view :
    Item l t ->
        Map ( View msg ( Item l t ) l Data )
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
                >> add_view.flow flow
                                            

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


serialize : Item l t -> String
serialize item =
    case item of
        Assume _ -> "<"

        -- case 1: final body
        Assume ( Self ref ) ->
            "<" ++name++": "

        -- case 2: body of certain group
        Body name flow ( Of ( head, tail ) End ) ->
            "Body: "++name++": "++
                ( head::tail
                    |> List.map ( from_codomain >> serialize )
                    |> String.join ", "
                )
                        
        -- case 3: body of ambiguous group
        Body name flow ( alternatives ) ->
            "Ambi: "++name++": "++
                ( alternatives
                  |> map_alternatives
                      ( \may_nonempty_cogroup ->
                            case may_nonempty_cogroup of
                                Just ( head, tail ) ->
                                    head::tail
                                       |> List.map ( from_codomain >> serialize )
                                       |> String.join ", "
                                _-> ""
                      )
                  |> String.join " || "     
                )

        Info s -> "🛈 " ++ s
        Err s -> "⚠ " ++ s
        T ( Text tx t ) -> "Text = " ++ Data.serialize tx ++ " = " ++ Data.serialize t
        P ( Picture picture ) -> "Picture = " ++ Data.serialize picture
        U ( Url url ) -> "Url = " ++ Data.serialize url
        Y ( Youtube youtube ) -> "Youtube = " ++ Data.serialize youtube
        V ( Vimeo vimeo ) -> "Vimeo = " ++ Data.serialize vimeo

          
deserialize : String -> Maybe ( Item l t )
deserialize str =
    Just <| case str of
        "<" ->
            Assume ( Self End )

        _ ->
            case String.split ": " str of
                "Body"::name::rest ->
                    String.join ": " rest
                        |> String.split ", "
                        |> List.map deserialize
                        |> List.foldl keep_just []
                        |> Body name Inside
                 
                _ ->
                     case String.split " = " str of
                         "Text"::da::ta ->
                             String.join " = " ta 
                                 |> Data.deserialize
                                 |> Text ( Data.deserialize da ) >> T
                         "Picture"::dat ->
                             String.join " = " dat
                                 |> Data.deserialize
                                 |> Picture >> I
                         "Url"::dat ->
                             String.join " = " dat
                                 |> Data.deserialize
                                 |> Url >> U
                         "Youtube"::dat ->
                             String.join " = " dat
                                 |> Data.deserialize
                                 |> Youtube >> Y
                         "Vimeo"::dat ->
                             String.join " = " dat
                                 |> Data.deserialize
                                 |> Vimeo >> V
                         _ ->
                             if String.startsWith "🛈 " str
                                 then String.dropLeft 3 str |> Info
                             else if String.startsWith "⚠ " str
                                 then String.dropLeft 2 str |> Err
                                 else "unable to deserialize "++str |> Err 
                             


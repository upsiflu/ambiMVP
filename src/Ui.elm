module Ui exposing
    ( view )
        
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing ( .. )

import Helpers exposing (..)

import History exposing ( History )
import History.Transformation exposing ( Transformation )
import History.Intent exposing ( .. )

import Svg exposing (svg) 
import Svg.Attributes exposing ( viewBox, d, fill )

{-  VIEW

    messages:
       browse_history: how to browse inside the History.
       intend: how to add a Transformation to the History.
    history: just a history.
    preview: how to turn a state within the history into Html.
    modifications: how to continue the history (create a subsequent transformation).
-}

view : { messages
       | transform : Transformation s -> msg, browse_history : Maybe Int -> msg
       , toggle_layout : msg, toggle_review : msg
       } ->
       { options
       | layout : Bool, editor : Bool, review : Bool, browse_past : Maybe Int
       } ->
       History s ->
       ( ( Intent s -> msg ) ->
         s ->
         List ( Html msg )
       ) ->
       List  ( Intent s ) ->
       { body : List (Html msg), title : String }

view messages options history preview possible_intents =
    let
        options_as_classes =
            classList [ -- on small screens, you toggle the review-view:
                        ( "reviewing", options.review )
                        -- for each item, you can be either visiting or .editing:
                      , ( "editing",   options.editor ) 
                        -- when the layout is complex, switch to list view:
                      , ( "listing",   options.editor && ( not options.layout ) )
                      ]
        summary  =
            History.summary history
        view_transformation ( sig, edit ) =
            span []
                [ span [ class "edit" ] 
                       [ text edit, span [ class "sig"  ] [ text sig  ] ] 
                ]
        
        view_avatar =
            header
                [ class "avatar" ]
                [ div 
                    [ class "avatar_picture" ] [ text "Avatar 0" ]
                , input 
                    [ value "upsiflu@gmail.com" ] []
                , section 
                    [ class "options" ]
                    [ button 
                        [ options_as_classes
                        , class "editor_mode", onClick messages.toggle_layout ] 
                        [ text "edit" 
                        , label [ class "layout_indicator"] [ text "list view " ] ]
                    , button 
                        [ options_as_classes
                        , class "review_mode", onClick messages.toggle_review ] 
                        [ text "review" ]
                    ]
                ]
        
        view_editor =
            let intent_to_message = History.do history >> messages.transform
            in 
            section 
                [ class "editor" ]
                ( preview intent_to_message summary.present )
                       
        view_review =
            section
                [ class "review" ]
                [ ( case options.browse_past of
                    Nothing ->
                        button 
                        [ class "publish"]
                        [ icons.upload
                        , text "publish" ]
                    Just i ->
                        button 
                        [ class "browse to publish"]
                        [ icons.upload
                        , text ( "apply all "++( String.fromInt i )++" edits" )
                        ]
                  )
                , button 
                    [ class "scrollable"
                    , messages.browse_history (Just -1) |> onClick ]
                    ( List.map view_transformation summary.past )
                , button 
                    [ class "scrollable"
                    , messages.browse_history (Just 1)  |> onClick ]
                    ( List.map view_transformation summary.future )
                , br [] []
                ]
                     
    in
        { title = "Kai!"
        , body = 
              [ view_avatar
              , main_ 
                    [ options_as_classes ] 
                    [ view_editor, view_review ] 
              ]
         }

         
-- assets

icons = 
    { upload =
        svg 
            [ Svg.Attributes.width "24"
            , Svg.Attributes.height "24"
            , viewBox "0 0 24 24"
            ]
            [ Svg.path [ d "M0 0h24v24H0V0z", fill "none" ]
                       []
            , Svg.path [ d "M19.35 10.04C18.67 6.59 15.64 4 12 4 9.11 4 6.6 5.64 5.35 8.04 2.34 8.36 0 10.91 0 14c0 3.31 2.69 6 6 6h13c2.76 0 5-2.24 5-5 0-2.64-2.05-4.78-4.65-4.96zM14 13v4h-4v-4H7l5-5 5 5h-3z" ]
                       []
            ]
    }

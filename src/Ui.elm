module Ui exposing
    ( view
    , Message )

{-|# View

@docs view

# Messages

@docs Message

-}
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing ( .. )

import Helpers exposing (..)

import History exposing ( History )
import History.Transformation exposing ( Transformation )
import History.Intent exposing ( .. )

import Svg exposing (svg) 
import Svg.Attributes exposing ( viewBox, d, fill )

{-|- browse_history:
  how to browse inside the History.
- toggle_layout: 
  how to switch between the list view and the layouted view in the editor.
- toggle_editor: 
  how to exit or enter the editor. 
- from_intent: 
  how to add a Transformation Intention to the History.
- from_command: 
  how to execute any command (such as focus).
-}
type alias Message state msg =
    { browse_history : ( Maybe Int ) -> msg
    , toggle_layout : msg
    , toggle_review : msg
    , from_intent : Intent state -> msg
    , from_command : Cmd msg -> msg
    , noop : msg
    }

{-|## [`message`](Ui#Message)

    { browse_history = BrowseHistory                                         
    , toggle_layout = ToggleLayout                                           
    , toggle_review = ToggleReview                                           
    , from_intent = History.do model.session >> Transform                    
    , from_command = DoCommand                                               
    , noop = NoOp                                                            
    }                                                                        

## `-> option`
the current Ui config

## `-> history`
just a History.

## `-> preview`
how to turn a State within the History into Html.

## `->`
create a Ui with a title.
-}
view :
    Message state msg ->
    { layout : Bool --> if not layout, and if editor, then class .listing
    , editor : Bool --> class .editing
    , review : Bool --> class .reviewing
    , browse_past : ( Maybe Int )
    } ->
    History state ->
    ( Message state msg -> state -> List ( Html msg ) ) ->
    { title : String, body : List ( Html msg ) }
view message option history preview =
    let
        options_as_classes =
            classList [ ( "reviewing", option.review )
                      , ( "editing",   option.editor ) 
                      , ( "listing",   option.editor && ( not option.layout ) )
                      ]
        summary  =
            History.summary history
                
        view_transformation ( sig, edit ) =
            button
                [ class "transformation" ]
                [ label [ class "edit" ] [ text edit ]
                , label [ class "signature"  ] [ text sig  ] 
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
                        , class "editor_mode", onClick message.toggle_layout ] 
                        [ text "edit" 
                        , label [ class "layout_indicator"] [ text "list view " ] ]
                    , button 
                        [ options_as_classes
                        , class "review_mode", onClick message.toggle_review ] 
                        [ text "review" ]
                    ]
                ]
        
        view_editor =
            section 
                [ class "editor" ]
                ( preview message summary.present )
                       
        view_review =
            section
                [ class "review" ]
                [ ( case option.browse_past of
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
                    [ class "past"
                    , message.browse_history (Just -1) |> onClick ]
                    ( List.map view_transformation ( List.reverse summary.past ) )
                , button 
                    [ class "future"
                    , message.browse_history (Just 1)  |> onClick ]
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

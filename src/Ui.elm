module Ui exposing
    ( view )
        
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing ( .. )

import Helpers exposing (..)

import History exposing ( History )
import History.Transformation exposing ( Transformation )
import History.Intent exposing ( .. )

{-  VIEW

    messages:
       browse_history: how to browse inside the History.
       intend: how to add a Transformation to the History.
    history: just a history.
    preview: how to turn a state within the history into Html.
    modifications: how to continue the history (create a subsequent transformation).
-}

view : { transform : Transformation s -> msg, browse_history : Maybe Int -> msg } ->
       History s ->
       ( ( Intent s -> msg ) ->
         s -> 
         Html msg
       ) ->
       List  ( Intent s ) ->
       { body : List (Html msg), title : String }

view messages history preview possible_intents =
    let
        summary  =
            History.summary history
        option_button copy =
            button
                 [ class "opt", messages.transform ( History.do history copy ) |> onClick ]
                 [ text copy.serial ]
        options  =
            possible_intents
                 |> List.map option_button
        view_transformation ( sig, edit ) =
            span []
                [ span [ class "edit" ] [ text edit, span [ class "sig"  ] [ text sig  ] ] ]
    in
        { title = "Kai!"
         , body =
               [ p   [ class "history" ]
                     [ button [ class "scrollable"
                              , messages.browse_history (Just -1) |> onClick ]
                              ( List.map view_transformation summary.past )
                     , button [ class "scrollable"
                              , messages.browse_history (Just 1)  |> onClick ]
                              ( List.map view_transformation summary.future )
                     , br [] []
                     , button [ class "hovering", messages.browse_history (Nothing) |> onClick ]
                              [ text "go to end of history" ]
                     ]
               , p  [ class "two_up" ] <| 
                       let
                             intent_to_message = History.do history >> messages.transform
                          in 
                             [ section [ class "tree" ] 
                                       [ preview intent_to_message summary.present ],
                               section [ class "layout" ] 
                                       [ preview intent_to_message summary.present ]
                             ]
                       
               ] ++ options 
         } 

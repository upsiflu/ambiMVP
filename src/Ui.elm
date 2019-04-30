module Ui exposing
    ( view )
        
import Html exposing (..)
import Html.Events exposing (..)

import Helpers exposing (..)
import History

import State exposing ( serialize, possibleTransformations )
import Transformation exposing ( do, undo )



view actions m =
    let
        summary =    m.composition |> History.summary
        (inc, mul) = do >> ( History.beyond m.composition ) |> possibleTransformations                   
                      
        --undo =       m.persist |> History.beyond >> undo
    in
        { title = "MVP"
         , body =
               [ p []
                     [ button [ actions.browseHistory (Just -1) |> onClick ] [ text <| ( String.join "<" ( ""::summary.past ) ) ++ "-" ]
                     , button [ actions.browseHistory (Just 1)  |> onClick ] [ text <| "-" ++ ( String.join ">" ( ""::summary.future ) ) ]
                     , br [] []
                     , button [ actions.browseHistory (Nothing)  |> onClick ] [ text "visit the end of history" ]
                     --, button [ actions.insert undo |> onClick ] [ text "undo" ]
                     ]
               , h1 [] [ text "MVP 2 (still! But undo is on the go)" ]
               , h2 [] [ text "state:" ], h2 [] [ summary.state |> serialize |> text ]
               , button [ actions.insert mul |> onClick ] [ text "×2" ]
               , button [ actions.insert inc |> onClick ] [ text "+1" ]
               ]
         } 

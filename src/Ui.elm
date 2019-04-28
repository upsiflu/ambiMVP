module Ui exposing
    ( view )
        
import Html exposing (..)
import Html.Events exposing (..)

import Helpers exposing (..)
import History

import State exposing ( serialize, possibleTransformations )


view actions m =
    let
        history =    m.persist |> History.view
        state =      m.persist |> History.state
        (inc, mul) = m.persist |> History.own >> possibleTransformations
    in
        { title = "MVP"
         , body =
               [ p []
                     [ button [ actions.browseHistory (Just -1) |> onClick ] [ text <| ( String.join "<" ( ""::history.past ) ) ++ "-" ]
                     , button [ actions.browseHistory (Just 1)  |> onClick ] [ text <| "-" ++ ( String.join ">" ( ""::history.future ) ) ]
                     , br [] []
                     , button [ actions.browseHistory (Nothing)  |> onClick ] [ text "visit the end of history" ]
                     ]
               , h1 [] [ text "MVP 2: Trivial, but now with a browseable State History" ]
               , h2 [] [ text "state:" ], h2 [] [ state |> serialize |> text ]
               , button [ actions.insert mul |> onClick ] [ text "×2" ]
               , button [ actions.insert inc |> onClick ] [ text "+1" ]
               ]
         } 

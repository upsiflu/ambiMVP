module Ui exposing (view)
import Browser
import Html exposing (..)
import Html.Events exposing (..)

import History exposing ( History )
import Route exposing ( Route )
import State exposing ( State, serialize, possibleTransformations )
import Transformation exposing ( Transformation )


view actions m =
    let
        ( state, (inc, dec), history ) = m.persist |> three ( History.state >> serialize, History.transformation >> possibleTransformations, History.view )
    in
        { title = "MVP"
         , body =
               [ p []
                     [ button [ actions.browseHistory (Just -1) |> onClick ] [ text <| ( String.join "<" history.past ) ++ "/" ]
                     , button [ actions.browseHistory (Just 1)  |> onClick ] [ text <| "/" ++ ( String.join ">" history.future ) ]
                     , br [] []
                     , button [ actions.browseHistory (Nothing)  |> onClick ] [ text "visit the end of history" ]
                     ]
               , h1 [] [ text "MVP 2: Trivial, but now with a browseable State History" ]
               , h2 [] [ text "state:" ], h2 [] [ state |> text ]
               , button [ actions.insert dec |> onClick ] [ text "-" ]
               , button [ actions.insert inc |> onClick ] [ text "+" ]
               ]
         }

            
-- helper

both ( f, g ) a = ( f a, g a )
three (f, g, h ) a = ( f a, g a, h a )

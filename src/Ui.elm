module Ui exposing (view)
import Html exposing (..)
import Browser

import Route exposing ( Route )
import State exposing ( State )

view : { m | route : Route, state : State } -> { title : String, body : List ( Html msg ) }
view m = { title = "MVP", body = [ h1 [] [ text "MVP 1: Trivial" ] ] }

module Main exposing (..)

-- general imports
import Browser
import Browser.Navigation as Nav
import Url exposing ( Url )
import Html exposing (..)

-- helper libs
import History

-- applicationlayers
import State exposing ( current )
import Route
import Ui



       
-- INIT


init : () -> Url.Url -> Nav.Key -> ( { state : {}, route : {} }, Cmd Msg )
init = \flags url key -> ( { state = State.trivial, route = Route.trivial }, Cmd.none )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url

update msg model =
    ( model, Cmd.none )

        

-- PROGRAM


main = Browser.application
    { init = init 
    , view = Ui.view
    , update = update
    , subscriptions = always Sub.none
    , onUrlChange = UrlChanged, onUrlRequest = LinkClicked
    }

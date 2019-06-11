module Main exposing (..)

-- general imports
import Browser
import Browser.Navigation as Nav
import Url exposing ( Url )
import Html exposing (..)

-- helper libs
import History exposing ( History )
import Helpers

-- applicationlayers
import State exposing ( State )
import Route exposing ( Route )
import Ui
import Transformation exposing ( Transformation )


       
-- INIT


init : () -> Url.Url -> Nav.Key -> ( { persist : History State, route : Route }, Cmd Msg )
init = \flags url key -> ( { persist = State.trivial |> History.singleton, route = Route.trivial }, Cmd.none )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Insert ( Transformation State )
    | BrowseHistory ( Maybe Int )
      
update msg model =
    case msg of
        Insert transformation -> ( { model | persist = model.persist |> History.insert transformation }, Cmd.none )
        BrowseHistory by -> ( { model | persist = model.persist |> History.browse by }, Cmd.none )
        _ -> ( model, Cmd.none )

-- PROGRAM


main = Browser.application
    { init = init 
    , view = Ui.view { insert = Insert, browseHistory = BrowseHistory }
    , update = update
    , subscriptions = always Sub.none
    , onUrlChange = UrlChanged, onUrlRequest = LinkClicked
    }
    

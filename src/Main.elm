module Main exposing (..)

-- platform
import Browser
import Browser.Navigation as Nav
import Url exposing ( Url )
import Html exposing (..)

-- functional libs
import History exposing ( History )
import Helpers
import Ui

-- persistent data structures
import State exposing ( State )
import Route exposing ( Route )
import Transformation exposing ( Transformation )


       
-- INIT

init : () -> Url.Url -> Nav.Key -> ( { composition : History State, route : Route }, Cmd Msg )
init = \flags url key -> ( { composition = History.singleton State.trivial, route = Route.trivial }, Cmd.none )



-- UPDATE

type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Insert ( Transformation State )
    | BrowseHistory ( Maybe Int )

update msg model =
    let persist f =
             ( { model | composition = f model.composition }, Cmd.none )
    in
    case msg of
        Insert transformation ->
            persist ( History.insert transformation )
        BrowseHistory by ->
            persist ( History.browse by )
        _ ->
            ( model, Cmd.none )

             
-- PROGRAM

main = Browser.application
    { init = init 
    , view = Ui.view { insert = Insert, browseHistory = BrowseHistory }
    , update = update
    , subscriptions = always Sub.none
    , onUrlChange = UrlChanged, onUrlRequest = LinkClicked
    }

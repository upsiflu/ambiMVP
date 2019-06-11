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
import Compositron as State exposing ( State )
import History.Transformation exposing ( Transformation )



-- types

type alias Route = String

       
-- init

init : () -> Url.Url -> Nav.Key -> ( { composition : History State, route : Route }, Cmd Msg )
init = \flags url key -> ( { composition = History.singleton State.trivial, route = "" }, Cmd.none )



-- update

type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Transform ( Transformation State )
    | BrowseHistory ( Maybe Int )
                 
update msg model =
    let
        persist f =
             ( { model | composition = f model.composition }, Cmd.none )
        browse_present = History.browse Nothing
    in
    case msg of
        Transform transformation ->
            persist ( History.insert transformation >> browse_present )
        BrowseHistory by ->
            persist ( History.browse by )
        _ ->
            ( model, Cmd.none )

             
-- program

main = Browser.application
    { init = init 
    , view = \model ->
             let
                 present = History.summary model.composition |> .present
                 recent_signature_string = History.recent_signature_string model.composition  
             in
                 Ui.view
                 { transform = Transform
                 , browse_history = BrowseHistory
                 }
                 model.composition
                 ( State.preview recent_signature_string )
                 ( State.possible_transformations recent_signature_string present )
    , update = update
    , subscriptions = always Sub.none
    , onUrlChange = UrlChanged, onUrlRequest = LinkClicked
    }

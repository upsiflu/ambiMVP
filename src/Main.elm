module Main exposing (..)

-- platform
import Browser
import Browser.Navigation as Nav
import Url exposing ( Url )
import Html exposing (..)

-- functional libs
import History exposing ( History )
import History.Intent exposing ( .. )
import Helpers
import Ui

-- persistent data structures
import Compositron as State exposing ( State )
import History.Transformation exposing ( Transformation )



-- types

type alias Route = String

       
-- init

init : () -> Url.Url -> Nav.Key -> ( { session : History State, route : Route }, Cmd Msg )
init = \flags url key -> ( { session = History.singleton State.trivial, route = "" }, Cmd.none )



-- update

type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Intend        ( Intent State )
    | Transform     ( Transformation State )
    | BrowseHistory ( Maybe Int )
                 
update msg model =
    let
        persist f =
             ( { model | session = f model.session }, Cmd.none )
        browse_present = History.browse Nothing
    in
    case msg of
        Intend intent ->
            -- package, send and receive
            persist identity
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
                 present = History.summary model.session |> .present
                 recent_signature_string = History.recent_signature_string model.session  
             in
                 Ui.view
                 { transform = Transform
                 , browse_history = BrowseHistory
                 }
                 model.session
                 ( State.preview recent_signature_string )
                 ( State.possible_intents recent_signature_string present )
    , update = update
    , subscriptions = always Sub.none
    , onUrlChange = UrlChanged, onUrlRequest = LinkClicked
    }

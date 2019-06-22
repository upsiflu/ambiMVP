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

type alias Model =
    { session : History State
    , route : Route
    , view_options : ViewOptions
    }

type alias ViewOptions =
    { layout : Bool
    , editor : Bool
    , review : Bool
    , browse_past : Maybe Int } 

init : () ->
    Url.Url ->
    Nav.Key ->
    ( Model
    , Cmd Msg 
    )
init = \flags url key ->
    ( { session = History.singleton State.trivial
      , route = ""
      , view_options =
        { layout = True
        , editor = True
        , review = False 
        , browse_past = Nothing
        }
      }, Cmd.none )



-- update

type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Intend        ( Intent State )
    | Transform     ( Transformation State )
    | BrowseHistory ( Maybe Int )
    | ToggleLayout
    | ToggleReview
                 
update msg model =
    let
        commit : ( Model -> Model ) -> ( Model, Cmd msg )
        commit f =
             ( model 
               |> f 
               |> ( \m -> map_session ( History.browse_to m.view_options.browse_past ) m )
             , Cmd.none )
        map_session : ( History State -> History State ) -> Model -> Model
        map_session f m =
             { m | session = f m.session }
        map_view : ( ViewOptions -> ViewOptions ) -> Model -> Model
        map_view f m =
             { m | view_options = f m.view_options }
        browse to =
             map_view ( \v-> {v| browse_past = to } )
    in
    case msg of
        Intend intent ->
            -- package, send and receive
            commit identity
        Transform transformation ->
            map_session ( History.insert transformation )
            >> browse Nothing --i.e. present
            |> commit 
        BrowseHistory to ->
            browse to
            |> commit
        ToggleLayout ->
            map_view (\v -> 
                if v.review 
                then {v| review = not v.review } 
                else {v| layout = not v.layout })
            |> commit
        ToggleReview ->
            map_view (\v -> {v| review = not v.review } )
            |> commit
        _ ->
            commit identity

             
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
                 , toggle_layout = ToggleLayout
                 , toggle_review = ToggleReview
                 }
                 model.view_options
                 model.session
                 ( State.preview recent_signature_string )
                 ( State.possible_intents recent_signature_string present )
    , update = update
    , subscriptions = always Sub.none
    , onUrlChange = UrlChanged, onUrlRequest = LinkClicked
    }

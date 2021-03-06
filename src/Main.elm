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
import Ui as Ui exposing ( Ui )

-- persistent data structures
import Compositron as State exposing ( State )
import History.Transformation exposing ( Transformation )

import Compositron.Signature as Signature 
import Helpers exposing (..)

-- types

type alias Route = String

       
-- init

type alias Model =
    { session : History State
    , route : Route
    , ui : Ui
    }

live =
 """
LIVE            	▓ :live/0
  < initial/0		  :live/1
 """ |> autocomplete
    
template =
    """
TEMPLATE			⮾ :_/0
  < ¶/0 | ❦/0	 		  :initial/0

  Paragraph			¶ :¶/0
    < ℌ/0 | ¶/0 | 𝑇/0		  :¶/1
    <				  :¶/2

  Figure			❦ :❦/0
    < blocks/1 | spans/1	  :❦/1
    Caption			ℭ :❦/2
      < blocks/1 | spans/1	  :❦/3
    < layout/1			  :❦/4

  heading			ℌ :ℌ/0
    < 𝑇/0			  :ℌ/1
    <				  :ℌ/2

  Layout			⍛ :layout/0
    < x/0 | x/1			  :layout/1
    <  | y/0			  :layout/2
    < z/0 | z/1 | z/2		  :layout/3
  center			⍛ :x/0
  align right			⍛ :x/1
  shade red			⍛ :y/0
  bottom			⍛ :z/0
  middle			⍛ :z/1
  top				⍛ :z/2

  T E X T			⮾ :_/0
    𝑇 fluid text ~ frozen text	  :𝑇/0
    < layout/0			  :𝑇/1
""" |> autocomplete

autocomplete =
    String.lines
        >> List.map ( \s ->
                          case String.split ":" s |> List.reverse of
                              sig_equal_proto::x::y -> s++"	("++sig_equal_proto++")"
                              _ -> s
                    )
        >> String.join "\n"

sig =
    Signature.deserialize >> Maybe.withDefault Signature.root

state =
    State.deserialize
        { live = live
        , template = template
        }
        |> State.target ( sig "live/1" )
        |> State.choose "ROOT" ( sig "initial/0", [] )
        |> State.choose "a" ( sig "¶/0", [] )
        -- initialized --
        {-
        |> State.choose "USERa" [ sig "blocks/1" ]
        |> State.target ( sig "USERa/0" )
        |> State.choose "USERb" [ sig "spans/1" ]
        |> State.choose "USERc" [ sig "text/1" ]
        -}
{-           
        |> State.target ( sig "USER a/4" )
        |> State.choose "USER b" [ sig "y/1" ]
        |> State.target ( sig "USER b/0" )
        |> State.choose "USER c" [ sig "y/0" ]
        |> State.target ( sig "USER a/1" )
        |> State.choose "USER d" [ sig "blocks/1" ]
        |> State.choose "USER e" [ sig "figure/0" ]
        |> State.choose "ERROR" [ sig "_/0" ]
-}

{--}
init : () ->
    Url.Url ->
    Nav.Key ->
    ( Model
    , Cmd Msg 
    )
init = \flags url key ->
    ( { session = History.singleton state
      , route = ""
      , ui = Ui.default
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
    | NoOp
    | DoCommand     ( Cmd Msg )
                 
update msg model =
    let
        commit : ( Model -> Model ) -> ( Model, Cmd msg )
        commit f =
            ( f model 
               |> ( \m ->
                      map_session
                        ( History.browse_to m.ui.browse_past ) m
                  )
            , Cmd.none )
            
        map_session : Map ( History State ) -> Map Model
        map_session f m =
            { m | session = f m.session }

        map_view : Map Ui -> Map Model
        map_view f m =
            { m | ui = f m.ui }

        browse to =
            map_view ( \v-> {v| browse_past = to } )
    in
    case msg of
        Intend intent ->
            -- package, send and receive
            commit identity
            |> trace "intend..."
        Transform transformation ->
            map_session ( History.insert transformation )
            >> browse Nothing --i.e. present
            |> commit 
            |> trace "transformation..."
        BrowseHistory to ->
            browse to
            |> commit
        ToggleLayout ->
            map_view (\v -> 
                if v.review 
                then { v | review = not v.review } 
                else { v | layout = not v.layout })
            |> commit
        ToggleReview ->
            map_view (\v -> {v| review = not v.review } )
            |> commit
        DoCommand c ->
            ( model, c )
            |> trace "do command..."
            |> \_ -> ( model, {-Debug.log "command"-} c )
        NoOp ->
            ( model, Cmd.none )
            |> trace "noop..."
        _ ->
            commit identity
            |> trace "identity..."

             
-- program

main = Browser.application
    { init = init 
    , view = \model->
        let
            present = History.summary model.session |> .present
            recent_signature_string = History.recent_signature_string model.session  
        in
            Ui.view
                { browse_history = BrowseHistory
                , toggle_layout = ToggleLayout
                , toggle_review = ToggleReview
                , from_intent = \i -> History.do i model.session |> Transform
                , from_command = DoCommand
                , noop = NoOp
                }
                model.ui
                model.session
                ( State.view recent_signature_string )
    , update = update
    , subscriptions = always Sub.none
    , onUrlChange = UrlChanged, onUrlRequest = LinkClicked
    }

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

{-| The template in ambilang would be:

Paragraph ¶ <
  < block
  < span
    
  layout
Figure <
  < block
  < span
  Caption
    < block
    < span
  layout
...
-}
template =
    """
TEMPLATE			⮾ :_/0	(_/0)
  < paragraph/0 | figure/0	  :initial/1	(initial/1)
  Paragraph			¶ :paragraph/0	(paragraph/0)
    ¶				⍛ :_/0	(_/0)
    < blocks/1 | spans/1 | layout/1	  :p/1	(p/1)
    <				  :p/2	(p/2)
  Figure			❦ :figure/0	(figure/0)
    Figure			⍛ :_/0	(_/0)
    < blocks/1 | spans/1	  :f/1	(f/1)
    Caption			ℭ :f/2	(f/2)
      < blocks/1 | spans/1	  :f/3	(f/3)
    <				  :f/4	(layout/1)
  Text				⟷ :t/0	(t/0)
    𝑇				𝑇 fluid text ~ frozen text :text/1	(text/1)
    <                             :t/3	(layout/1)
  L A Y O U T			⮾ :_/0	(_/0)
    Layout...			🜺 :x/0	(x/0)
    < y/0 | y/1 | y/2		  :layout/1	(layout/1)
  align right			⍛ :y/0	(y/0)
  align centered		⍛ :y/1	(y/1)
  shaded green			⍛ :y/2	(y/2)
  layout			⍛ :l/0	(l/0)
  B L O C K S  A N D  S P A N S	⮾ :_/0	(_/0)
    Block elements...		🜺 :_/0	(_/0)
    < paragraph/0 | figure/0 | heading/1	 :blocks/1	(blocks/1)
  insert span element		⮾ :_/0	(_/0)
    Runs of text...		🜺 :_/0	(_/0)
    < text/1 | spans/1		  :spans/1	(spans/1)
  heading			⍞ :h/0	(h/0)
    Heading			⍛ :_/0	(_/0)
    <				  :heading/1	(heading/1)
    <				  :h/2	(spans/1)
    <				  :h/3	(layout/1)
  link				⟷ :k/0	(k/0)
    destination			⍛ :link/1	(link/1)
      U 			  :k/2	(k/2)"""

   
live =
 """
LIVE            	▓ :live/0	(live/0)
  <			  :live/1	(initial/1)*
 """
    

sig =
    Signature.deserialize >> Maybe.withDefault Signature.root
    
state =
    State.deserialize
        { live = live
        , template = template
        }
        |> State.target ( sig "live/1" )
        |> State.choose "ROOT" ( sig "initial/1", [] )
        -- initialized --
        --|> State.choose "USERa" [ sig "blocks/1" ]
        --|> State.choose "USERa" [ sig "paragraph/0" ]
        --|> State.target ( sig "USERa/0" )
        --|> State.choose "USERb" [ sig "spans/1" ]
        --|> State.choose "USERc" [ sig "text/1" ]

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

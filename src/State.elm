module State exposing
    ( State, trivial, serialize, possibleTransformations, Transformations )

import Transformation exposing
    ( Transformation, invert )


type State = State { count : Int }




{-  STATE

    A data structure that will eventually be synced with a server.

    At this stage, we implement a counter.
    Its history is managed by Main.elm.
-}


-- initialize an empty state.
trivial : State
trivial = State { count = 0 }


-- Endofunctions:
increment : State -> State 
increment ( State s ) = State { count = s.count+1 }

decrement : State -> State
decrement ( State s ) = State { count = s.count-1 }


-- Possible Transformations after the recent one:
-- we export all transformations possible "after" a preceding transformation.
type alias Transformations = ( Transformation State, Transformation State )
possibleTransformations : Transformation State -> Transformations
possibleTransformations after = Transformation.create "Inc" increment decrement after |> both ( identity, invert )

                        
-- serialize
serialize ( State s ) = s.count |> String.fromInt


                        
-- helper

both ( f, g ) a = ( f a, g a )

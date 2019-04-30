module State exposing
    ( State                    -- create
    , trivial 
    , serialize                -- present
    , possibleTransformations  -- permutations
    )

import Helpers exposing (..)

type State = State { count : Float }
    


-- create

trivial : State
trivial = State { count = 0 }


          
-- modify

increment : State -> State 
increment ( State s ) = State { count = s.count+1 }

decrement : State -> State
decrement ( State s ) = State { count = s.count-1 }

double : State -> State
double ( State s ) = State { count = s.count*2 }

half : State -> State
half ( State s ) = State { count = s.count/2 }
                                                                                                
possibleTransformations assemble =
    ( assemble { serial = "Inc", function = increment, inverse = decrement }
    , assemble { serial = "Mul", function = double, inverse = half }
    )


        
-- present

serialize ( State s ) = s.count |> round |> String.fromInt


                        

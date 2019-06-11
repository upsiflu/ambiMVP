module State exposing
    ( State
    , trivial 
    , preview
    , possible_transformations  -- all valid permutations
    )

import Html exposing ( .. )

import Helpers exposing ( .. )

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
                                                                                 
possible_transformations _ =
    [ { serial = "Inc", function = increment, inverse = decrement }
    , { serial = "Mul", function = double, inverse = half }
    ]


-- read

count ( State s ) = s.count

        
-- present

preview : State ->
          ( { serial : String, function : Endofunction, inverse : Endofunction } -> msg ) ->
          Html msg
preview state dispatch = state |> count >> round >> String.fromInt >> text


                        

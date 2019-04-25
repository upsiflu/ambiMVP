module State exposing ( State, trivial, current )

type alias State = {}

-- initialize an empty state.
trivial : State
trivial = {}

-- output the current state out of the history of states.
current = always trivial

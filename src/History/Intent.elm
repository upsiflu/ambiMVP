module History.Intent exposing
    ( Intent
    , Endofunction )

{-|
# Definition
@docs Intent

# Types
@docs Endofunction
-}

{-|-}
type alias Endofunction s =
    s -> s

{-|-}
type alias Intent s =
    { serial : String, function : Endofunction s, inverse : Endofunction s }

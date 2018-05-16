module Checkers where

import Prelude

data Player = Red | Black | Neither
type Position = {x :: Number, y :: Number}
type State = Array (Array Player)

instance eqPlayer :: Eq Player where
    eq Red Red = true
    eq Black Black = true
    eq _ _ = false
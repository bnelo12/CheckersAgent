module Checkers where

import Prelude
import Data.Tuple (Tuple(..))

data Player = Red | Black | Neither
type Position = {x :: Number, y :: Number}
type State = Array (Array Player)
type Point = Tuple Int Int
type Model = {state :: State, mouse :: Point, turn :: Player}

instance eqPlayer :: Eq Player where
    eq Red Red = true
    eq Black Black = true
    eq _ _ = false
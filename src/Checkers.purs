module Checkers where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))

data Player = Red | Black | Neither
type Position = {x :: Number, y :: Number}
type State = Array (Array Player)
type Eight a = {_0 :: a, _1 :: a, _2 :: a, _3 :: a, _4 :: a, _5 :: a, _6 :: a, _7 :: a}
type Board = Eight (Eight Player)
type Point = Tuple Int Int
type Model = {board :: Board, turn :: Player, selected :: Maybe Position, agentThinking :: Boolean}

instance eqPlayer :: Eq Player where
    eq Red Red = true
    eq Black Black = true
    eq Neither Neither = false
    eq _ _ = false

getEightValue :: ∀ a. Int -> Eight a -> a
getEightValue 0 b = b._0
getEightValue 1 b = b._1
getEightValue 2 b = b._2
getEightValue 3 b = b._3
getEightValue 4 b = b._4
getEightValue 5 b = b._5
getEightValue 6 b = b._6
getEightValue 7 b = b._7
getEightValue _ b = b._0

getBoardState :: Int -> Int -> Board -> Player
getBoardState x y b = getEightValue y (getEightValue x b)
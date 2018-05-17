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
type Model = {state :: Board, mouse :: Point, turn :: Player, selected :: Maybe Position, agentThinking :: Boolean}

instance eqPlayer :: Eq Player where
    eq Red Red = true
    eq Black Black = true
    eq _ _ = false

getBoardState :: Int -> Int -> Board -> Player
getBoardState 0 0 b = b._0._0
getBoardState 0 1 b = b._0._1
getBoardState 0 2 b = b._0._2
getBoardState 0 3 b = b._0._3
getBoardState 0 4 b = b._0._4
getBoardState 0 5 b = b._0._5
getBoardState 0 6 b = b._0._6
getBoardState 0 7 b = b._0._7
getBoardState 1 0 b = b._1._0
getBoardState 1 1 b = b._1._1
getBoardState 1 2 b = b._1._2
getBoardState 1 3 b = b._1._3
getBoardState 1 4 b = b._1._4
getBoardState 1 5 b = b._1._5
getBoardState 1 6 b = b._1._6
getBoardState 1 7 b = b._1._7
getBoardState 2 0 b = b._2._0
getBoardState 2 1 b = b._2._1
getBoardState 2 2 b = b._2._2
getBoardState 2 3 b = b._2._3
getBoardState 2 4 b = b._2._4
getBoardState 2 5 b = b._2._5
getBoardState 2 6 b = b._2._6
getBoardState 2 7 b = b._2._7
getBoardState 3 0 b = b._3._0
getBoardState 3 1 b = b._3._1
getBoardState 3 2 b = b._3._2
getBoardState 3 3 b = b._3._3
getBoardState 3 4 b = b._3._4
getBoardState 3 5 b = b._3._5
getBoardState 3 6 b = b._3._6
getBoardState 3 7 b = b._3._7
getBoardState 4 0 b = b._4._0
getBoardState 4 1 b = b._4._1
getBoardState 4 2 b = b._4._2
getBoardState 4 3 b = b._4._3
getBoardState 4 4 b = b._4._4
getBoardState 4 5 b = b._4._5
getBoardState 4 6 b = b._4._6
getBoardState 4 7 b = b._4._7
getBoardState 5 0 b = b._5._0
getBoardState 5 1 b = b._5._1
getBoardState 5 2 b = b._5._2
getBoardState 5 3 b = b._5._3
getBoardState 5 4 b = b._5._4
getBoardState 5 5 b = b._5._5
getBoardState 5 6 b = b._5._6
getBoardState 5 7 b = b._5._7
getBoardState 6 0 b = b._6._0
getBoardState 6 1 b = b._6._1
getBoardState 6 2 b = b._6._2
getBoardState 6 3 b = b._6._3
getBoardState 6 4 b = b._6._4
getBoardState 6 5 b = b._6._5
getBoardState 6 6 b = b._6._6
getBoardState 6 7 b = b._6._7
getBoardState 7 0 b = b._7._0
getBoardState 7 1 b = b._7._1
getBoardState 7 2 b = b._7._2
getBoardState 7 3 b = b._7._3
getBoardState 7 4 b = b._7._4
getBoardState 7 5 b = b._7._5
getBoardState 7 6 b = b._7._6
getBoardState 7 7 b = b._7._7
getBoardState _ _ _ = Neither
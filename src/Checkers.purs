module Checkers where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Monoid (class Monoid, mempty)

data Player = Red | Black | Neither
type Position = {x :: Number, y :: Number}
newtype Eight a = Eight {_0 :: a, _1 :: a, _2 :: a, _3 :: a, _4 :: a, _5 :: a, _6 :: a, _7 :: a}
newtype Board = Board (Eight (Eight Player))
type Point = Tuple Int Int
type Model = {board :: Board, turn :: Player, selected :: Maybe Position, agentThinking :: Boolean}

instance eqPlayer :: Eq Player where
    eq Red Red = true
    eq Black Black = true
    eq Neither Neither = false
    eq _ _ = false

instance showPlayer :: Show Player where
    show Red = "R"
    show Black = "B"
    show Neither = "N"

instance showEight :: (Show a) => Show (Eight a) where
    show (Eight b) =
        (show b._0) <>
        " " <> (show b._1) <>
        " " <> (show b._2) <>
        " " <> (show b._3) <>
        " " <> (show b._4) <>
        " " <> (show b._5) <>
        " " <> (show b._6) <>
        " " <> (show b._7)

instance showBoard :: Show Board where
    show (Board b) = foldl (\acc x -> acc <> (show x) <> "\n") "" b 

instance foldableEight ::  Foldable Eight where
    foldl f acc (Eight b) = f (f (f (f (f (f (f (f acc b._0) b._1) b._2) b._3) b._4) b._5) b._6) b._7
    foldr f acc (Eight b) = f b._7 $ f b._6 $ f b._5 $ f b._4 $ f b._3 $ f b._2 $ f b._1 $ f b._0 acc
    foldMap f b = foldl append mempty (map f b)

instance semigroupEight :: Semigroup a => Semigroup (Eight a) where
    append (Eight a) (Eight b) =
        Eight {
            _0: append a._0 b._0,
            _1: append a._1 b._1, 
            _2: append a._2 b._2,
            _3: append a._3 b._3,
            _4: append a._4 b._4,
            _5: append a._5 b._5,
            _6: append a._6 b._6,
            _7: append a._7 b._7
        }

instance monoidEight :: Monoid a => Monoid (Eight a) where
    mempty = Eight {
            _0: mempty,
            _1: mempty, 
            _2: mempty,
            _3: mempty,
            _4: mempty,
            _5: mempty,
            _6: mempty,
            _7: mempty
        }

instance functorEight :: Functor Eight where
    map f (Eight b) = 
        Eight {
            _0: f b._0,
            _1: f b._1,
            _2: f b._2,
            _3: f b._3,
            _4: f b._4,
            _5: f b._5,
            _6: f b._6,
            _7: f b._7
        }

getEightValue :: ∀ a. Int -> Eight a -> a
getEightValue 0 (Eight b) = b._0
getEightValue 1 (Eight b) = b._1
getEightValue 2 (Eight b) = b._2
getEightValue 3 (Eight b) = b._3
getEightValue 4 (Eight b) = b._4
getEightValue 5 (Eight b) = b._5
getEightValue 6 (Eight b) = b._6
getEightValue 7 (Eight b) = b._7
getEightValue _ (Eight b) = b._0

getBoardState :: Int -> Int -> Board -> Player
getBoardState x y (Board b) = getEightValue y (getEightValue x b)
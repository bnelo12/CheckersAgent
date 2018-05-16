module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, getCanvasElementById, getContext2D)
import Checkers (Player(Red, Black, Neither), State)
import Checkers.World (drawBoard, drawState)

import Partial.Unsafe (unsafePartial)

initialState :: State
initialState = [
                [Neither,  Black, Neither,  Black,  Neither,  Black,  Neither,  Black],
                [ Black,  Neither,  Black,  Neither,  Black,  Neither,  Black,  Neither],
                [ Neither,  Black,  Neither,  Black,  Neither,  Black,  Neither,  Black],
                [ Neither,  Neither,  Neither,  Neither,  Neither,  Neither,  Neither,  Neither],
                [ Neither,  Neither,  Neither,  Neither,  Neither,  Neither,  Neither,  Neither],
                [Red,  Neither, Red,  Neither, Red,  Neither, Red,  Neither],
                [ Neither, Red,  Neither, Red,  Neither, Red,  Neither, Red],
                [Red,  Neither, Red,  Neither, Red,  Neither, Red,  Neither]
              ]

main :: Eff (canvas :: CANVAS) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  _ <- drawBoard ctx
  drawState initialState ctx
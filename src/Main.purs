module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, random)
import Data.Array ((..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, Context2D, strokePath, fillPath, arc, setStrokeStyle,
                        setFillStyle, getContext2D, getCanvasElementById, fillRect)

import Math as Math
import Partial.Unsafe (unsafePartial)

main :: Eff (canvas :: CANVAS, random :: RANDOM) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  drawBoard ctx

drawBoard :: forall eff. Context2D -> Eff (canvas :: CANVAS | eff) Unit
drawBoard ctx = void do
  _ <- setFillStyle "#8c5242" ctx
  _ <- fillRect ctx {x: 0.0, y: 0.0, w: 600.0, h: 600.0}
  for_ [0.0, 2.0, 4.0, 6.0] \x -> do
    for_ [0.0, 2.0, 4.0, 6.0] \y -> do
      _ <- setFillStyle "#ffffce" ctx
      _ <- fillRect ctx {x: x*75.0, y: y*75.0, w: 75.0, h: 75.0}
      fillRect ctx {x: (x+1.0)*75.0, y: (y+1.0)*75.0, w: 75.0, h: 75.0}


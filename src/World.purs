module Checkers.World where

import Prelude
import Control.Monad.Eff (Eff)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..))
import Data.Foldable (for_)
import Data.Int (toNumber)
import Graphics.Canvas (CANVAS, Context2D, setFillStyle, fillRect, arc, fillPath, getCanvasElementById, getContext2D)

import Math as Math

import Checkers (Player(Red, Black, Neither), Position, State, Model, Point)

boardWidth :: Number
boardWidth = 600.0

boardHeight :: Number
boardHeight = 600.0

cellWidth :: Number
cellWidth = 75.0

drawBoard :: ∀ eff. Context2D -> Eff (canvas :: CANVAS | eff) Unit
drawBoard ctx = void do
  _ <- setFillStyle "#8c5242" ctx
  _ <- fillRect ctx {x: 0.0, y: 0.0, w: boardWidth, h: boardHeight}
  for_ [0.0, 2.0, 4.0, 6.0] \x -> do
    for_ [0.0, 2.0, 4.0, 6.0] \y -> do
      _ <- setFillStyle "#ffffce" ctx
      _ <- fillRect ctx {x: x*cellWidth, y: y*cellWidth, w: cellWidth, h: cellWidth}
      fillRect ctx {x: (x+1.0)*cellWidth, y: (y+1.0)*cellWidth, w: cellWidth, h: cellWidth}

drawPiece :: ∀ eff. Player -> Position -> Context2D -> Eff (canvas :: CANVAS | eff) Unit
drawPiece player pos ctx = void do
  _ <- if player == Red 
    then setFillStyle "#ff0000" ctx else setFillStyle "#000000" ctx
  let path = arc ctx {x: pos.x*cellWidth + cellWidth/2.0, y: pos.y*cellWidth + cellWidth/2.0, r: 28.0, start: 0.0, end: Math.pi * 2.0}
  fillPath ctx path

drawState :: ∀ eff. State -> Context2D -> Eff (canvas :: CANVAS | eff) Unit
drawState state ctx = void do
  forWithIndex_ state \i row -> do
    forWithIndex_ row \j s -> do
      case s of
        Black -> drawPiece Black {x: toNumber j, y: toNumber i} ctx
        Red -> drawPiece Red {x: toNumber j, y: toNumber i} ctx
        Neither -> pure unit

render :: ∀ eff. Partial => Model -> (Eff (canvas :: CANVAS | eff) Unit)
render model =
  void do
    Just canvas <- getCanvasElementById "canvas"
    ctx <- getContext2D canvas
    _ <- drawBoard ctx
    drawState model.state ctx

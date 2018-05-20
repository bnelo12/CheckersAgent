module Checkers.World where

import Prelude
import Data.Array ((..))
import Control.Monad.Eff (Eff)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), maybe)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Graphics.Canvas (CANVAS, Context2D, setFillStyle, setStrokeStyle, strokePath, fillRect, arc, fillPath, getCanvasElementById, getContext2D)
import Control.Monad.Eff.Ref (REF, Ref(..), readRef, modifyRef, newRef)
import Partial.Unsafe (unsafePartial)

import Math as Math

import Checkers (Player(Red, Black, Neither), Position, Model, Point, Board, getBoardState)

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

drawState :: ∀ eff. Board -> Context2D -> Eff (canvas :: CANVAS | eff) Unit
drawState board ctx = void do
  for_ (0..7) \i -> do
    for_ (0..7) \j -> do
      case (getBoardState i j board) of
        Neither -> pure unit
        Red -> drawPiece Red {x: toNumber j, y: toNumber i} ctx
        Black -> drawPiece Black {x: toNumber j, y: toNumber i} ctx

drawSelector :: ∀ eff. Context2D -> Position -> Eff (canvas :: CANVAS | eff) Unit
drawSelector ctx pos = void do
  _ <- setStrokeStyle "#f4d942" ctx
  let path = arc ctx {x: pos.x*cellWidth + cellWidth/2.0, y: pos.y*cellWidth + cellWidth/2.0, r: 28.0, start: 0.0, end: Math.pi * 2.0}
  strokePath ctx path

render :: ∀ eff. Partial => Ref Model -> (Eff _ Unit)
render ref =
  void do
    Just canvas <- getCanvasElementById "canvas"
    ctx <- getContext2D canvas
    _ <- drawBoard ctx
    model <- readRef ref
    _ <- drawState model.board ctx
    maybe (pure unit) (drawSelector ctx) model.selected

update :: ∀ eff.  Ref Model -> (Eff _ Unit)
update ref = do
  model <- readRef ref
  if model.agentThinking 
    then pure unit
    else unsafePartial $ render ref



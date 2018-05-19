module Checkers.Event where

import Prelude

import Checkers (Model(..), Player(..), getBoardState)
import Checkers.World (boardWidth, boardHeight)
import Control.Monad.Eff (Eff, runPure)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (runExcept)
import Control.Monad.Eff.Ref (REF, Ref(..), readRef, modifyRef, newRef)
import Data.Either (Either(..), either)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), fromJust, maybe)
import DOM (DOM)
import DOM.Event.MouseEvent (eventToMouseEvent, pageX, pageY)
import DOM.Event.Types (Event, MouseEvent)
import DOM.HTML (window)
import DOM.HTML.Window (innerWidth)
import Control.Monad.Eff.Ref (REF, readRef, writeRef, newRef)
import Checkers.World (update)


toMouseEvent :: Event -> Maybe MouseEvent
toMouseEvent = either (const Nothing) Just <<< runExcept <<< eventToMouseEvent

windowXCoordToGameXCoord :: ∀ eff. Int -> Eff (dom :: DOM | eff) Int
windowXCoordToGameXCoord x = do 
    map (\y -> (x - (y - (round boardWidth))/2)/ (round (boardWidth/8.0)) ) (liftEff $ innerWidth =<< window)

windowYCoordToGameYCoord :: Int -> Int
windowYCoordToGameYCoord y = (y - 8)/(round (boardHeight/8.0))

onMouseClick :: Ref Model -> Event -> Eff _ Unit
onMouseClick ref ev = do
  let mev = toMouseEvent ev
  let x' = maybe 0 pageX mev
  let y = windowYCoordToGameYCoord (maybe 0 pageY mev)
  x <- windowXCoordToGameXCoord x'
  ref' <- readRef ref
  let newSelected = if (getBoardState y x ref'.board) == Red then Just {x: toNumber x, y: toNumber y} else Nothing
  _ <- writeRef ref {board: ref'.board, turn: ref'.turn, selected: newSelected, agentThinking: ref'.agentThinking} 
  update ref
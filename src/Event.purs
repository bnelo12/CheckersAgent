module Checkers.Event where

import Prelude

import Checkers (Model(..))
import Checkers.World (boardWidth, boardHeight)
import Control.Monad.Eff (Eff, runPure)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (runExcept)
import Control.Monad.Eff.Ref (REF, Ref(..), readRef, modifyRef, newRef)
import Data.Either (Either(..), either)
import Data.Int (round)
import Data.Maybe (Maybe(..), fromJust, maybe)
import DOM (DOM)
import DOM.Event.MouseEvent (eventToMouseEvent, pageX, pageY)
import DOM.Event.Types (Event, MouseEvent)
import DOM.HTML (window)
import DOM.HTML.Window (innerWidth)
import Control.Monad.Eff.Ref (REF, readRef, modifyRef, newRef)


toMouseEvent :: Event -> Maybe MouseEvent
toMouseEvent = either (const Nothing) Just <<< runExcept <<< eventToMouseEvent

windowXCoordToGameXCoord :: ∀ eff. Int -> Eff (dom :: DOM | eff) Int
windowXCoordToGameXCoord x = do 
    map (\y -> (x - (y - (round boardWidth))/2)/ (round (boardWidth/8.0)) ) (liftEff $ innerWidth =<< window)

windowYCoordToGameYCoord :: Int -> Int
windowYCoordToGameYCoord y = (y - 8)/(round (boardHeight/8.0))

onMouseClick :: Event -> Eff _ Unit
onMouseClick ev = do
  let mev = toMouseEvent ev
  let x' = maybe 0 pageX mev
  let y = windowYCoordToGameYCoord (maybe 0 pageY mev)
  x <- windowXCoordToGameXCoord x'
  log ((show x) <> " " <> (show y))
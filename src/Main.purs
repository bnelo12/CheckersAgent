module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Checkers (Player(Red, Black, Neither), State, Model)
import DOM.Event.EventTarget
import DOM.Event.Types
import DOM.HTML.Document (body)
import DOM.Node.Types
import DOM.Event.MouseEvent
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.HTML.Types
import Control.Monad.Except (runExcept)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Classy.Event.EventTarget (toEventTarget)
import DOM.HTML.Event.EventTypes (click)
import DOM.Node.ParentNode (QuerySelector(..), querySelector)
import Control.Monad.Eff.Ref (REF, readRef, modifyRef, newRef)
import Partial.Unsafe (unsafePartial)
import Checkers.World (render)
import Data.Tuple (Tuple(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Either (Either(..), either)
import Control.Monad.Eff.Ref (REF, readRef, modifyRef, newRef)
import Unsafe.Coerce (unsafeCoerce)

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

initialModel :: Model
initialModel = {state: initialState, mouse: Tuple 1 1, turn: Red}

toMouseEvent :: forall a. Event -> Maybe MouseEvent
toMouseEvent = either (const Nothing) Just <<< runExcept <<< eventToMouseEvent

onMouseClick :: Event -> Eff _ Unit
onMouseClick ev = do
  let x = maybe 0 pageX (toMouseEvent ev)
  log (show x)

main :: Eff (_) Unit
main = void $ unsafePartial do
  document <- htmlDocumentToDocument <$> (document =<< window)
  canvasEl :: Maybe Element <- getElementById  (ElementId "canvas") (documentToNonElementParentNode document)
  let canvasEl' = toEventTarget (unsafePartial (fromJust canvasEl))
  addEventListener click (eventListener onMouseClick) false canvasEl'
  render initialModel


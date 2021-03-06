module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Checkers (Player(Red, Black, Neither), Model, Board(..), Eight(..))
import Checkers.Event (onMouseClick)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.Types (Element, ElementId(ElementId), documentToNonElementParentNode)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Classy.Event.EventTarget (toEventTarget)
import DOM.HTML.Event.EventTypes (click)
import Partial.Unsafe (unsafePartial)
import Checkers.World (render)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), fromJust)
import Control.Monad.Eff.Ref (REF, Ref(..), readRef, modifyRef, newRef)


initialBoard :: Board
initialBoard = Board (Eight {
                  _0: (Eight {_0: Neither, _1: Black, _2: Neither, _3: Black, _4: Neither, _5: Black, _6: Neither, _7: Black}),
                  _1: (Eight {_0: Black, _1: Neither, _2: Black, _3: Neither, _4: Black, _5: Neither, _6: Black, _7: Neither}),
                  _2: (Eight {_0: Neither, _1: Black, _2: Neither, _3: Black, _4: Neither, _5: Black, _6: Neither, _7: Black}),
                  _3: (Eight {_0: Neither, _1: Neither, _2: Neither, _3: Neither, _4: Neither, _5: Neither, _6: Neither, _7: Neither}),
                  _4: (Eight {_0: Neither, _1: Neither, _2: Neither, _3: Neither, _4: Neither, _5: Neither, _6: Neither, _7: Neither}),
                  _5: (Eight {_0: Red, _1: Neither, _2: Red, _3: Neither, _4: Red, _5: Neither, _6: Red, _7: Neither}),
                  _6: (Eight {_0: Neither, _1: Red, _2: Neither, _3: Red, _4: Neither, _5: Red, _6: Neither, _7: Red}),
                  _7: (Eight {_0: Red, _1: Neither, _2: Red, _3: Neither, _4: Red, _5: Neither, _6: Red, _7: Neither})
                })

initialModel :: Model
initialModel = {board: initialBoard, turn: Red, selected: Nothing, agentThinking: false}

main :: Eff (_) Unit
main = void $ unsafePartial do
  ref :: Ref Model <- newRef initialModel
  document <- htmlDocumentToDocument <$> (document =<< window)
  canvas :: Maybe Element <- getElementById  (ElementId "canvas") (documentToNonElementParentNode document)
  let canvas' = toEventTarget (unsafePartial (fromJust canvas))
  addEventListener click (eventListener (onMouseClick ref)) false canvas'
  render ref

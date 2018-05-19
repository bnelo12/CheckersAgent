module Checkers.Search where

import Prelude
import Checkers (Board, Player(..))

-- Takes a current board state and player
-- and returns all posible actiojns that
-- player can take.
actions :: Board -> Player -> Array Board
actions _ Neither = []
actions b p = [b]
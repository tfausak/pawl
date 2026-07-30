module Pawl.Types.Deck where

import Data.Map.Strict (Map)
import Numeric.Natural (Natural)
import Pawl.Types.Printing (Printing)

-- A deck is a multiset of printings: a shuffle erases any order among the cards,
-- so counts are the honest model. `Printing` and everything beneath it derive
-- `Ord`, so it is a lawful `Map` key.
newtype Deck = MkDeck (Map Printing Natural)
  deriving (Eq, Show)

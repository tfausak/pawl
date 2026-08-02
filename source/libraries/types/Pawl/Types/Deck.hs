module Pawl.Types.Deck where

import qualified Data.Map.Strict as Map
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Printing as Printing

-- | A deck is a multiset of printings: a shuffle erases any order among the cards,
-- so counts are the honest model. `Printing` and everything beneath it derive
-- `Ord`, so it is a lawful `Map` key.
newtype Deck = MkDeck {unwrap :: Map.Map Printing.Printing Natural.Natural}
  deriving (Eq, Show)

module Pawl.Types.PlayerDrawsNthCard where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.PlayerRelation as PlayerRelation

-- | CR 603.2 over CR 121's draw: whose draw counts, and WHICH draw of the turn
-- fires the ability -- Erudite Wizard's "second card". No ordinal is every draw,
-- Teferi, Hero of Dominaria's emblem's "whenever you draw a card".
data PlayerDrawsNthCard = MkPlayerDrawsNthCard
  { player :: PlayerRelation.PlayerRelation,
    nth :: Maybe Natural.Natural
  }
  deriving (Eq, Ord, Show)

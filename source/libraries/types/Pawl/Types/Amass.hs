module Pawl.Types.Amass where

import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Subtype as Subtype

-- | The payload of Pawl.Types.Effect's Amass arm: CR 701.47a's two printed
-- variables, "amass [subtype] N".
--
-- The subtype is a printed WORD, so CR 612.1 reaches it and
-- Pawl.Engine.Projection.rewriteEffect swaps it; the Army type beside it is the
-- rulebook's and appears nowhere here. CR 701.47d's older amass without a subtype
-- has Oracle errata reading "amass Zombies N", so every printing writes one.
--
-- N is a Quantity rather than a Natural: Summons of Saruman
-- prints "amass Orcs X" and Saruman, the White Hand "amass Orcs X, where X is that
-- spell's mana value", so the count is an expression over game state.
data Amass = MkAmass
  { quantity :: Quantity.Quantity,
    subtype :: Subtype.Subtype
  }
  deriving (Eq, Ord, Show)

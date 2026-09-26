module Pawl.Types.ForetellCost where

import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.ManaCost as ManaCost

-- | The payload of Pawl.Types.Keyword's Foretell arm: CR 702.143a's [cost], the
-- one a foretold card is later CAST for.
--
-- PARAMETRIC in the keyword for Pawl.Types.Cycling's reason: a Cost can name a
-- Keyword, and Keyword names this. Only @ForetellCost Keyword.Keyword@ is ever
-- written.
data ForetellCost keyword
  = -- | CR 702.143a: the cost the card states -- foretell {1}{U}.
    Stated (Cost.Cost keyword)
  | -- | CR 702.143a / 118.7: the receiving card's own mana cost reduced by this
    -- amount -- Dream Devourer's "its foretell cost is equal to its mana cost
    -- reduced by {2}". Settled per face at the cast (CR 712.11b), off the
    -- foretold card's Object.foretellCostReduction stamp.
    ManaCostReducedBy ManaCost.ManaCost
  deriving (Eq, Ord, Show)

module Pawl.Types.ExileMaterials where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Filter as Filter

-- | The payload of Pawl.Types.CostComponent's ExileMaterials arm: CR 702.167a's
-- "Exile [materials] from among permanents you control and\/or cards in your
-- graveyard", exiling this many matching objects out of the two zones taken
-- together.
--
-- PARAMETRIC in the keyword for the Filter it carries, exactly as
-- Pawl.Types.ExileCardsFromGraveyard is.
--
-- count is Pawl.Types.ExileCardsFromGraveyard's reading: the objects are chosen
-- and counted exactly. The Filter is matched against each candidate's CR 613
-- projection on both sides at once, which is CR 702.167b's exception to rule
-- 109.2 -- "creature" admits a creature on the battlefield and a creature card
-- in a graveyard, and the ONE criterion read over the two pools is what says so.
data ExileMaterials keyword = MkExileMaterials
  { count :: Natural.Natural,
    whichObjects :: Filter.Filter keyword
  }
  deriving (Eq, Ord, Show)

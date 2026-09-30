module Pawl.Types.Behold where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Filter as Filter

-- | The payload of Pawl.Types.CostComponent's Behold arm: CR 701.4a as a cost,
-- beholding exactly this many DISTINCT objects the Filter admits -- Caustic
-- Exhale's "a Dragon" is one, Kindle the Inner Flame's "three Elementals" three.
--
-- PARAMETRIC in the keyword for the Filter it carries, exactly as
-- Pawl.Types.CostComponent is. Pawl.Types.ExileMaterials' field names, the
-- pool spanning two zones there as here.
data Behold keyword = MkBehold
  { count :: Natural.Natural,
    whichObjects :: Filter.Filter keyword
  }
  deriving (Eq, Ord, Show)

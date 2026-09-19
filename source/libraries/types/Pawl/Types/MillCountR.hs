module Pawl.Types.MillCountR where

import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.MillCountRewrite as MillCountRewrite

-- | The payload of Pawl.Types.ReplacementEffect's MillCountR arm: whose mill
-- INSTRUCTIONS the row watches (CR 701.17a / 614.1a), and what happens instead.
--
-- A bare ControllerRelation and no threshold field, Pawl.Types.LifeGainR's shape
-- rather than Pawl.Types.DrawCountR's: Alms Collector's "two or more" needs a
-- threshold and no printed mill clause narrows that way -- Bruvac the
-- Grandiloquent and The Water Crystal both say "one or more", which every
-- instruction that mills at all already meets.
data MillCountR = MkMillCountR
  { whose :: ControllerRelation.ControllerRelation,
    rewrite :: MillCountRewrite.MillCountRewrite
  }
  deriving (Eq, Ord, Show)

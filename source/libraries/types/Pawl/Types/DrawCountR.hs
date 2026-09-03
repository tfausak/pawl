module Pawl.Types.DrawCountR where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.DrawCountRewrite as DrawCountRewrite

-- | The payload of Pawl.Types.ReplacementEffect's DrawCountR arm: whose draw
-- INSTRUCTIONS the row watches (CR 121.2a), how many cards one has to name before
-- the row applies, and what happens instead.
--
-- A record where Pawl.Types.DrawR is a bare ControllerRelation, and the extra
-- field is the whole difference between the two event classes: an INSTRUCTION
-- carries a number, which is what CR 121.2a's "replacement effects that refer to
-- the number of cards drawn" refer to.
data DrawCountR = MkDrawCountR
  { whose :: ControllerRelation.ControllerRelation,
    -- | CR 614.1a: Alms Collector's "two or more". An instruction naming fewer
    -- cards does not meet the row's condition, so it never reaches CR 616.1's
    -- choice and is never spent under CR 614.5.
    atLeast :: Natural.Natural,
    rewrite :: DrawCountRewrite.DrawCountRewrite
  }
  deriving (Eq, Ord, Show)

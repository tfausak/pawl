module Pawl.Types.Emerge where

import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Filter as Filter

-- | The payload of Pawl.Types.Keyword's Emerge arm: CR 702.119a's "Emerge
-- [cost]", plus CR 702.119b's "emerge from [quality]" as one field on it.
--
-- PARAMETRIC in the keyword, for Pawl.Types.Equip's reason: the fields name a
-- Cost and a Filter, both of which can name a Keyword, and Keyword names THIS.
-- Only @Emerge Keyword.Keyword@ is ever written.
--
-- quality is Nothing for rule 702.119a, whose victim is a creature, and Just for
-- rule 702.119b, whose victim is a [quality] PERMANENT. One field rather than a
-- second Keyword constructor because rule 702.119b changes only the pool
-- Pawl.Engine.Cost.candidateCostsGiven draws the sacrifice from: the cost, the
-- CR 601.2f reduction by the victim's mana value, and CR 702.119c's choice are
-- word for word rule 702.119a's.
data Emerge keyword = MkEmerge
  { cost :: Cost.Cost keyword,
    quality :: Maybe (Filter.Filter keyword)
  }
  deriving (Eq, Ord, Show)

module Pawl.Types.TargetCount where

import qualified Numeric.Natural as Natural

-- | CR 601.2c: HOW MANY targets ONE instance of the word "target" takes -- a
-- range, `least` through `most`. The four printed phrasings are four ranges:
-- "target creature" is 1 to 1, CR 115.6's "up to one target creature" is 0 to 1,
-- "up to two target creatures" is 0 to 2, and "one or two target creatures"
-- (Hearts on Fire) is 1 to 2.
--
-- A RANGE and not a sum of those phrasings, because CR 601.2c states the rule
-- that way: "if the spell has a variable number of targets, the player announces
-- how many targets they will choose", and the number announced must be one the
-- printed words allow. `least == most` is that rule's "in some cases, the number
-- of targets will be defined by the spell's text" -- nothing to announce.
--
-- `least <= most` and `1 <= most` are invariants nothing in this module
-- maintains: a slot is card DATA, so Pawl.Codec.TargetCount rejects a range that
-- breaks either. Neither is a safety property -- an impossible range makes a mode
-- unfillable rather than making anything crash.
data TargetCount = MkTargetCount
  { least :: Natural.Natural,
    most :: Natural.Natural
  }
  deriving (Eq, Ord, Show)

-- The ordinary "target creature": exactly one, and no announcement.
one :: TargetCount
one = MkTargetCount {least = 1, most = 1}

-- CR 115.6's "up to N target creatures": zero through N.
upTo :: Natural.Natural -> TargetCount
upTo n = MkTargetCount {least = 0, most = n}

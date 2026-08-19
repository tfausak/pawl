module Pawl.Types.TargetCount where

import qualified Numeric.Natural as Natural

-- | CR 601.2c: HOW MANY targets ONE instance of the word "target" takes -- a
-- range, `least` through `most`. The printed phrasings are ranges: "target
-- creature" is 1 to 1, CR 115.6's "up to one target creature" is 0 to 1, "up to
-- two target creatures" is 0 to 2, "one or two target creatures" (Hearts on
-- Fire) is 1 to 2, and "any number of target creatures, planeswalkers, and/or
-- players" (Soulfire Eruption) is 0 to NO printed maximum.
--
-- A RANGE and not a sum of those phrasings, because CR 601.2c states the rule
-- that way: "if the spell has a variable number of targets, the player announces
-- how many targets they will choose", and the number announced must be one the
-- printed words allow. `least == most` is that rule's "in some cases, the number
-- of targets will be defined by the spell's text" -- nothing to announce.
--
-- `most` is a Maybe because "any number" names no maximum at all. The BOARD is
-- then the only ceiling -- a caster cannot announce more targets than there are
-- legal recipients to choose (CR 601.2c) -- so every reader of `most` asks
-- `ceilingOn` with the candidate count rather than reading the field.
--
-- `least <= most` and `1 <= most` are invariants nothing in this module
-- maintains: a slot is card DATA, so Pawl.Codec.TargetCount rejects a range that
-- breaks either. Neither is a safety property -- an impossible range makes a mode
-- unfillable rather than making anything crash. An unbounded `most` satisfies
-- both by construction.
data TargetCount = MkTargetCount
  { least :: Natural.Natural,
    most :: Maybe Natural.Natural
  }
  deriving (Eq, Ord, Show)

-- The ordinary "target creature": exactly one, and no announcement.
one :: TargetCount
one = MkTargetCount {least = 1, most = Just 1}

-- CR 115.6's "up to N target creatures": zero through N.
upTo :: Natural.Natural -> TargetCount
upTo n = MkTargetCount {least = 0, most = Just n}

-- CR 601.2c's "any number of target ...": zero through whatever the board offers.
anyNumber :: TargetCount
anyNumber = MkTargetCount {least = 0, most = Nothing}

-- CR 601.2c: the most targets this count can be announced with on a board
-- offering `candidates` legal recipients -- the printed maximum where the card
-- states one, and the candidates themselves where it says "any number".
ceilingOn :: Natural.Natural -> TargetCount -> Natural.Natural
ceilingOn candidates = maybe candidates (min candidates) . most

-- May this count be answered with more than one target? True for "any number",
-- which states no maximum to compare against.
plural :: TargetCount -> Bool
plural = maybe True (> 1) . most

module Pawl.Types.ControlClock where

-- | How many of one player's upkeeps have begun since that player came to
-- control a permanent, counted to two and then stopped. Stored per player in
-- Pawl.Types.Object.controlClock, where a seat with no entry has never
-- controlled the permanent at all.
--
-- THREE states and not a flag. Rule 702.30a's window is "since the beginning of
-- your LAST upkeep" and the intervening "if" is read TWICE (CR 603.4, CR
-- 608.2a): a flag cleared as the upkeep begins would be gone by the resolution
-- that re-reads it, and a flag cleared when the step ends would forget a
-- permanent that came under your control DURING that upkeep, which rule 702.30a
-- still reaches at the next one. Advancing once per upkeep answers both --
-- `SinceLastUpkeep` holds for the whole of the upkeep it was promoted in. The
-- third state is what keeps `Elapsed` apart from "never controlled it", which
-- Pawl.Engine.Engine.sampleControlClock would otherwise read as a fresh gain
-- every time it looked.
--
-- Written by Pawl.Engine.Engine.sampleControlClock, which adds `Gained` for a
-- permanent's live controller the way Engine.sampleControl samples control, and
-- advanced by Engine.advanceControlClock at the beginning of that player's
-- upkeep.
data ControlClock
  = -- | CR 702.30a: this player came to control the permanent, and no upkeep of
    -- theirs has begun since.
    Gained
  | -- | CR 702.30a: this player came to control the permanent since the
    -- beginning of their last upkeep -- the state the rule's "if" is true in.
    SinceLastUpkeep
  | -- | CR 702.30a: longer ago than that, so the rule's "if" is false.
    Elapsed
  deriving (Bounded, Enum, Eq, Ord, Show)

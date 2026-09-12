module Pawl.Types.ControlClock where

-- | How far one player's CR 702.30a window has run on a permanent they came to
-- control: the window has opened and no upkeep of theirs has begun since, or
-- exactly one has. Stored per player in Pawl.Types.Object.controlClock, where a
-- seat with NO entry has no open window at all -- it has never controlled this
-- incarnation, or its window ran out. Deleted rather than kept as a third
-- "elapsed" state, so that "no window" has one representation, which is
-- Pawl.Types.GameState.landsPlayed's posture one type over.
--
-- TWO states and not a flag, because rule 702.30a's window is "since the
-- beginning of your LAST upkeep" and the intervening "if" is read TWICE (CR
-- 603.4, CR 608.2a). A flag cleared as the upkeep begins would be gone by the
-- resolution that re-reads it, and a flag cleared when the step ends would
-- forget a permanent that came under your control DURING that upkeep, which rule
-- 702.30a still reaches at the next one. Advancing the clock once per upkeep
-- answers both: `SinceLastUpkeep` holds for the whole of the upkeep it was
-- promoted in, and the entry is dropped at the upkeep after that.
--
-- Written by Pawl.Engine.Engine.sampleControl, which opens a window for the
-- player a permanent has just come under the control of, and advanced by
-- Engine.advanceControlClock at the beginning of that player's upkeep.
data ControlClock
  = -- | CR 702.30a: this player came to control the permanent, and no upkeep of
    -- theirs has begun since.
    Gained
  | -- | CR 702.30a: this player came to control the permanent since the
    -- beginning of their last upkeep -- the state the rule's "if" is true in.
    SinceLastUpkeep
  deriving (Bounded, Enum, Eq, Ord, Show)

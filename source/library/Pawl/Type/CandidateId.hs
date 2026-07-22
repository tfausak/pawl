module Pawl.Type.CandidateId where

import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.ReplacementEffect (ReplacementEffect)
import Pawl.Type.Timestamp (Timestamp)

-- CR 614.5's identity: "A replacement effect doesn't invoke itself repeatedly; it
-- gets only one opportunity to affect an event or any modified events that may
-- replace that event." This is what counts as ONE effect for that rule. Without
-- it Hardened Scales and Corpsejack Menace re-fire on each other's output
-- forever -- it is a termination condition, not an optimization.
--
-- A permanent's static replacement ability is identified by (source, effect
-- VALUE), NOT (source, list index). Index identity would break CR 616.2, the rule
-- this phase exists to get right: a Clone applies its own `EntryR AsCopy` (index
-- 0 of its one-element list), which replaces its copiable snapshot with a Primal
-- Plasma's -- whose `EntryR (ChoiceOf ...)` is then ALSO index 0. The
-- newly-acquired ability would be mistaken for the one already used, and the
-- Gatherer ruling's board state would be unreachable.
--
-- Two Doubling Seasons are still two opportunities: different SOURCES. The cost
-- of value identity is that a single source carrying two TEXTUALLY IDENTICAL
-- replacement abilities gets one opportunity instead of two; no card in the pool
-- does that (#75).
--
-- A floating replacement is identified by (source, timestamp): GameState's
-- timestamp counter is monotone, so two Fogs are two instances even from one
-- source object.
data CandidateId
  = OfPermanent ObjectId ReplacementEffect
  | OfFloating ObjectId Timestamp
  deriving (Eq, Ord, Show)

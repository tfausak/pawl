module Pawl.Types.TriggerFrequency where

-- | How often a trigger condition may match within one turn.
--
-- There is NO comprehensive rule for "for the first time each turn". The nearest,
-- CR 603.2h, qualifies an INSTRUCTION rather than the trigger EVENT. Aurelia, the
-- Warleader's phrase is plain card text narrowing a CR 508.3a trigger event, and
-- this type says so rather than manufacturing a citation.
--
-- Pawl.Types.TriggerLimit is the other half of that pair and NOT a synonym: it
-- rides the ABILITY and states "this ability triggers only once each turn",
-- where this narrows one condition's EVENT.
--
-- Load-bearing on the one card that carries it: Aurelia adds a combat phase when
-- she attacks (CR 500.8), so without the narrowing she attacks again in the phase
-- she added, adds another, and the turn never ends.
data TriggerFrequency
  = -- | CR 603.2c: every occurrence of the trigger event counts.
    EveryTime
  | -- | Only the first occurrence in a turn.
    FirstTimeEachTurn
  deriving (Bounded, Enum, Eq, Ord, Show)

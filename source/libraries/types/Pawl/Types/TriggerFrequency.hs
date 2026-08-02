module Pawl.Types.TriggerFrequency where

-- | How often a trigger condition may match within one turn.
--
-- There is NO comprehensive rule for "for the first time each turn". The
-- nearest, CR 603.2h, is about "Do this only once each turn" -- a qualifier on
-- an INSTRUCTION, gated on what the source's controller has already done -- which
-- is a different thing from a qualifier on the trigger EVENT. Aurelia, the
-- Warleader's phrase is plain card text narrowing a CR 508.3a trigger event, and
-- this type says so rather than manufacturing a citation for it.
--
-- Load-bearing on the one card that carries it, not decoration: Aurelia adds a
-- combat phase when she attacks (CR 500.8), so without the narrowing she attacks
-- again in the phase she added, adds another, and the turn never ends.
data TriggerFrequency
  = -- | CR 603.2c: "An ability triggers only once each time its trigger event
    -- occurs" -- and every occurrence counts. Hanweir Garrison's.
    EveryTime
  | -- | Only the first occurrence in a turn.
    FirstTimeEachTurn
  deriving (Eq, Ord, Show)

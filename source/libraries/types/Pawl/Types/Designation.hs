module Pawl.Types.Designation where

-- | A designation A PERMANENT can have and nothing else about it: CR 702.112b's
-- renowned, CR 701.37b's monstrous and CR 701.60b's suspected. All three rules
-- word the mark the same way -- "only permanents can" have it, it is "neither an
-- ability nor part of the permanent's copiable values", and it lasts until the
-- permanent leaves the battlefield -- so all three are one payload rather than
-- three fields, three opcodes and three read atoms (Pawl.Types.Object's
-- `designations`, Effect.Designate, Quantity.HasDesignation,
-- Filter.HasDesignation).
--
-- A payload and not three constructors apiece, by Pawl.Types.Scaling's argument:
-- what separates renown from monstrosity is WHICH mark, which is a value.
--
-- What the type does NOT cover is every other thing the CR calls a designation,
-- because none of them has this shape:
--
-- * CR 701.54b's Ring-bearer is per-player -- Object.ringBearerFor is a
--   `Maybe PlayerId`, and CR 701.54a ends it on a change of control, which none
--   of these three has.
--
-- * CR 725.1's monarch designates a PLAYER, so it lives on GameState.
--
-- * CR 731.1's day and night designate the GAME (Pawl.Types.Daytime).
--
-- Membership here says the mark is STORED and READ alike; it does not say the
-- three are interchangeable, and two places deliberately keep them apart. What
-- SETS one differs: CR 702.112a mints renown's trigger, CR 701.37a's monstrosity
-- is an activated ability's clause, and CR 701.60a's suspect is an instruction
-- another card gives. What READS one differs more: CR 701.60c hangs menace and
-- "this creature can't block" off `Suspected` alone (Pawl.Engine.Projection and
-- Pawl.Engine.CombatRestriction case on this constructor for it), and CR 701.60a
-- lets `Suspected` alone END before the permanent leaves the battlefield, which
-- is why Effect.Unsuspect is its own opcode rather than a designation-parameterised
-- inverse of Effect.Designate -- no rule takes renowned or monstrous away.
data Designation
  = -- | CR 702.112b: renowned, the marker rule 702.112a's renown ability sets.
    Renowned
  | -- | CR 701.37b: monstrous, the marker CR 701.37a's monstrosity action sets.
    Monstrous
  | -- | CR 701.60b: suspected, which CR 701.60a's suspect instruction sets and,
    -- unlike the two above, which a spell or ability can take away again.
    Suspected
  deriving (Bounded, Enum, Eq, Ord, Show)

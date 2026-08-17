module Pawl.Types.Designation where

-- | A designation A PERMANENT can have and nothing else about it: CR 702.112b's
-- renowned, CR 701.37b's monstrous, CR 701.60b's suspected and CR 719.3b's
-- solved. Every one of those rules words the mark the same way -- only a
-- permanent can have it, it is "neither an ability nor part of the permanent's
-- copiable values", and it lasts until the permanent leaves the battlefield --
-- so they are one payload rather than a field, an opcode and a read atom apiece
-- (Pawl.Types.Object's `designations`, Effect.Designate,
-- Quantity.HasDesignation, Filter.HasDesignation).
--
-- A payload and not a constructor apiece, by Pawl.Types.Scaling's argument:
-- what separates renown from monstrosity is WHICH mark, which is a value.
--
-- What the type does NOT cover is every other thing the CR calls a designation,
-- because none of them has this shape:
--
-- * CR 701.54b's Ring-bearer is per-player -- Object.ringBearerFor is a
--   `Maybe PlayerId`, and CR 701.54a ends it on a change of control, which none
--   of the marks here has.
--
-- * CR 725.1's monarch designates a PLAYER, so it lives on GameState.
--
-- * CR 731.1's day and night designate the GAME (Pawl.Types.Daytime).
--
-- Membership here says the mark is STORED and READ alike; it does not say the
-- marks are interchangeable, and two places deliberately keep them apart. What
-- SETS one differs: CR 702.112a mints renown's trigger, CR 701.37a's monstrosity
-- is an activated ability's clause, CR 701.60a's suspect is an instruction
-- another card gives, and CR 719.3a's "to solve" is an end-step trigger the Case
-- itself carries. What READS one differs more: CR 701.60c hangs menace and
-- "this creature can't block" off `Suspected` alone (Pawl.Engine.Projection and
-- Pawl.Engine.CombatRestriction case on this constructor for it), and CR 701.60a
-- lets `Suspected` alone END before the permanent leaves the battlefield, which
-- is why Effect.Unsuspect is its own opcode rather than a designation-parameterised
-- inverse of Effect.Designate -- no rule takes renowned, monstrous or solved away.
data Designation
  = -- | CR 702.112b: renowned, the marker rule 702.112a's renown ability sets.
    Renowned
  | -- | CR 701.37b: monstrous, the marker CR 701.37a's monstrosity action sets.
    Monstrous
  | -- | CR 701.60b: suspected, which CR 701.60a's suspect instruction sets and,
    -- unlike every other mark here, which a spell or ability can take away
    -- again.
    Suspected
  | -- | CR 719.3b: solved, the marker CR 719.3a's "to solve" trigger sets and
    -- CR 719.3c's "Solved --" ability is gated on. Renowned's shape: set by a
    -- trigger of the permanent's own, and with no remover.
    Solved
  deriving (Bounded, Enum, Eq, Ord, Show)

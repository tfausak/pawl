module Pawl.Types.Designation where

-- | A designation A PERMANENT can have and nothing else about it: CR 702.112b's
-- renowned, CR 701.37b's monstrous, CR 701.60b's suspected, CR 719.3b's solved,
-- CR 722.3b's prepared and CR 702.171b's saddled. Every one of those rules words
-- the mark the same way -- only a permanent can have it, and it is "neither an
-- ability nor part of the permanent's copiable values" -- so they are one
-- payload rather than a field, an opcode and a read atom apiece
-- (Pawl.Types.Object's `designations`, Effect.Designate,
-- Quantity.HasDesignation, Filter.HasDesignation).
--
-- WHERE THE MARK ENDS is the one axis they do not share, and it is a sweep
-- rather than a field: five of the six last until the permanent leaves the
-- battlefield, which CR 400.7's new object gives for free, and CR 702.171b adds
-- "or the end of the turn" to saddled alone -- Pawl.Engine.Expiry.dropAtCleanup's
-- clearedSaddles. That is not the objection that keeps goaded out below, which is
-- per-PLAYER as well as expiring.
--
-- CR 701.37c's X rides alongside rather than inside: Object.designationValues
-- keys a number by which mark set it, and Quantity.DesignationValue reads one
-- back. Only "monstrosity X" writes one, so it is a second field and not a
-- payload on this type.
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
-- * CR 702.131c's city's blessing and CR 702.195b's enduring story designate a
--   PLAYER too, and are each other's twin rather than the monarch's: both rules
--   let any number of players hold the mark at once, where CR 725.3 makes the
--   monarch unique, so they ride Player.designations as a set per seat
--   (Pawl.Types.PlayerDesignation).
--
-- * CR 731.1's day and night designate the GAME (Pawl.Types.Daytime).
--
-- * CR 701.15b's goaded is per-player AND expiring, the Ring-bearer's two
--   objections at once: Object.goadedBy is a set of seats, since CR 701.15c lets
--   several players goad one creature, and CR 701.15a ends each entry at that
--   player's next turn rather than when the permanent leaves the battlefield.
--
-- * CR 716.2b's level is a NUMBER -- "a level is a designation that any permanent
--   can have" -- so it is Object.classLevel, a Maybe Pawl.Types.ClassLevel, read
--   by Quantity.ClassLevel. A constructor here plus a designationValues entry
--   could not answer CR 716.2a's "level N or greater" either: CR 716.2d gives a
--   permanent that was never levelled a level of 1, where a mark nobody set has
--   no value at all.
--
-- Membership here says the mark is STORED and READ alike; it does not say the
-- marks are interchangeable, and two places deliberately keep them apart. What
-- SETS one differs: CR 702.112a mints renown's trigger, CR 701.37a's monstrosity
-- is an activated ability's clause, CR 701.60a's suspect is an instruction
-- another card gives, and CR 719.3a's "to solve" is an end-step trigger the Case
-- itself carries. What READS one differs more: CR 701.60c hangs menace and
-- "this creature can't block" off `Suspected` alone (Pawl.Engine.Projection and
-- Pawl.Engine.CombatRestriction case on this constructor for it), and CR 701.60a
-- lets a SPELL OR ABILITY end `Suspected`, which is why Effect.Unsuspect is its
-- own opcode rather than a designation-parameterised inverse of Effect.Designate
-- -- no rule takes renowned, monstrous, solved or saddled away, the last of those
-- ending on the clock instead.
-- `Prepared` is `Suspected`'s shape in the second respect and not the first: CR
-- 722.3c ends it too, at the moment the copy is cast, but no opcode takes it --
-- Pawl.Engine.Cast does, at CR 601.2i. What it adds that no other mark has is a
-- precondition on GAINING it and an object minted as it is gained, both
-- Pawl.Engine.Prepare's.
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
  | -- | CR 702.171b: saddled, the marker CR 702.171a's saddle ability sets on
    -- resolution. Renowned's shape in what sets it and the only mark here that
    -- also ends at end of turn -- see the note above.
    Saddled
  | -- | CR 722.3b: prepared, which CR 722.3a's "becomes prepared" instruction
    -- sets and which CR 722.3b takes away again -- Suspected's shape rather than
    -- Renowned's, and the only other mark here that a rule removes.
    --
    -- Two clauses of CR 722.3a are gates the other marks have no counterpart for,
    -- and Pawl.Engine.Prepare holds both: "a permanent can't gain this
    -- designation unless it has a prepare spell", and it may not gain one it
    -- already has -- which is Effect.Designate's standing transition guard.
    -- Gaining it is not only a write, either: CR 722.3c mints a copy of the
    -- permanent in exile at the same moment, which is
    -- Pawl.Engine.Prepare.mintOnDesignated, run in the same breath as the
    -- set-insert the other four take alone.
    Prepared
  deriving (Bounded, Enum, Eq, Ord, Show)

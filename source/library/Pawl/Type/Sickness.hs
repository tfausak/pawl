module Pawl.Type.Sickness where

-- CR 302.6: a creature can't attack, or use an activated ability with the tap
-- symbol, unless it has been under its controller's control continuously since
-- their most recent turn began.
--
-- A sum type rather than a Bool: no boolean blindness.
--
-- Set to Sick by changeZone (a new object has been controlled for zero time) and
-- cleared to Settled at the untap step. Control-change re-sickening is handled
-- (M4.5 P1): Resolve.applyEffect's GainControl arm re-sets Sick on the target
-- when control moves (CR 302.6 is about control held CONTINUOUSLY), and the
-- untap-step settle (Engine.settleAll, iterating Projection.controls) clears it
-- for whichever player controls the permanent at that step. Remaining
-- deferral: P1's control effects are all until-end-of-turn, so the thief never
-- reaches their own untap step with the creature still theirs; settling a
-- permanent held under INDEFINITE control across turns is the Auras / Control
-- Magic phase.
data Sickness
  = Sick
  | Settled
  deriving (Eq, Ord, Show)

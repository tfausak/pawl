module Pawl.Type.Sickness where

-- CR 302.6: a creature can't attack, or use an activated ability with the tap
-- symbol, unless it has been under its controller's control continuously since
-- their most recent turn began.
--
-- A sum type rather than a Bool: no boolean blindness.
--
-- Set to Sick by changeZone (a new object has been controlled for zero time) and
-- cleared to Settled at the untap step. EXPIRES at M3: CR 302.6 is about control
-- held CONTINUOUSLY, so a control change resets the clock and this must be
-- re-set when control moves.
data Sickness
  = Sick
  | Settled
  deriving (Eq, Ord, Show)

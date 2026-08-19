module Pawl.Types.Sickness where

import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 302.6: a creature can't attack, or use an activated ability with the tap or
-- untap symbol, unless it has been under its controller's control continuously
-- since their most recent turn began. Only the tap half is reachable -- no cost
-- component expresses the untap symbol (#204).
--
-- `Settled` names WHO the continuity claim is about, because CR 302.6's subject
-- is a player rather than the creature. A bare not-sick flag cannot answer that
-- and got it wrong for Control Magic (#198) -- the thief inherited the victim's
-- settle. A reader asks `sickness obj == Settled pid` for the specific player
-- whose turn and control are at issue.
--
-- Two writers, both moments CR 302.6 names: Engine.settleAll writes
-- `Settled pid` at pid's untap step for everything pid controls; and
-- Engine.checkControlContinuity DROPS a `Settled p` whose object p no longer
-- controls. Control is DERIVED, so nothing announces a change and the check
-- samples wherever the board can change -- the same diff Engine.sampleControl
-- takes for CR 603.2, taken again rather than read off the event log, for the
-- reason checkControlContinuity's own comment gives. It only ever clears, never
-- grants, so a stolen creature stays sick after the Aura leaves and control returns.
--
-- Everything else writes `Sick`, CR 400.7 making a zone change or a new token a
-- new object no player has controlled for any time. The exception is an object
-- built directly onto the stack or into the command zone -- an ability, an emblem
-- -- stamped `Settled` under its own controller: every reader is
-- battlefield-gated, so the value is inert rather than an assertion. A spell is
-- not in that group; it reaches the stack through Event.changeZone.
data Sickness
  = Sick
  | Settled PlayerId.PlayerId
  deriving (Eq, Ord, Show)

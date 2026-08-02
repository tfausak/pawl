module Pawl.Types.Sickness where

import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 302.6: a creature can't attack, or use an activated ability with the tap or
-- untap symbol, unless it has been under its controller's control continuously
-- since their most recent turn began. Only the tap half is reachable -- no cost
-- component expresses the untap symbol (#204).
--
-- `Settled` names WHO the continuity claim is about, because CR 302.6's subject
-- is a player, not the creature: "under ITS CONTROLLER'S control since THEIR most
-- recent turn began". A bare not-sick flag cannot answer that question, and got
-- it wrong for Control Magic (#198) -- the thief inherited the victim's settle.
-- A reader asks `sickness obj == Settled pid` for the specific player whose turn
-- and control are at issue, so the claim can never be read by the wrong player.
--
-- Two writers put the claim there, and both are moments CR 302.6 names:
--   * Engine.settleAll writes `Settled pid` at pid's untap step, for everything
--     pid controls then. That is literally "since their most recent turn began".
--   * Engine.checkControlContinuity DROPS a `Settled p` whose object p no longer
--     controls. Control is DERIVED (a control-granting static ability is re-read
--     live by the projection), so a change has no event to hook; it samples
--     instead, wherever the board can change. It only ever clears, never grants,
--     so a stolen creature stays sick even after the Aura leaves and control
--     returns.
--
-- Everything else writes `Sick`: Event.changeZone and Event.createToken, because
-- CR 400.7 makes each a new object no player has controlled for any time at all;
-- Pawl.Engine.Setup, for the cards a game starts with; and Resolve's GainControl arm,
-- because control just moved.
--
-- The exception is an object built directly onto the stack or into the command
-- zone -- an activated or triggered ability, an emblem -- which is stamped
-- `Settled` under its own controller. Summoning sickness is meaningless off the
-- battlefield, and every reader is battlefield-gated, so the value is inert
-- either way; it is not an assertion about anything. A spell is not in this
-- group: it reaches the stack through Event.changeZone, so it carries `Sick`.
data Sickness
  = Sick
  | Settled PlayerId.PlayerId
  deriving (Eq, Ord, Show)

module Pawl.Type.Sickness where

import Pawl.Type.PlayerId (PlayerId)

-- CR 302.6: a creature can't attack, or use an activated ability with the tap or
-- untap symbol, unless it has been under its controller's control continuously
-- since their most recent turn began.
--
-- `Settled` names WHO the continuity claim is about, because CR 302.6's subject
-- is a player, not the creature: "under ITS CONTROLLER'S control since THEIR most
-- recent turn began". A bare not-sick flag cannot answer that question, and got
-- it wrong for Control Magic (#198) -- the thief inherited the victim's settle.
-- A reader asks `sickness obj == Settled pid` for the specific player whose turn
-- and control are at issue, so the claim can never be read by the wrong player.
--
-- Writers, and why each is the moment CR 302.6 names:
--   * Event.changeZone sets Sick -- CR 400.7 makes the moved object a new one,
--     which no player has controlled for any time at all.
--   * Engine.settleAll sets `Settled pid` at pid's untap step, for everything pid
--     controls then. That is literally "since their most recent turn began".
--   * Resolve's GainControl arm sets Sick: control just moved (Act of Treason).
--   * Engine.checkControlContinuity DROPS a `Settled p` whose object p no longer
--     controls. Control is DERIVED (Projection.controllerOf reads Control Magic's
--     static ability), so a change has no event to hook; this samples instead, at
--     Engine.settleForPriority -- every point CR 117.5 lets a player observe the
--     board. It only ever clears, never grants, so a stolen creature stays sick
--     even after the Aura leaves and control returns.
--
-- An object on the stack or in the command zone carries `Settled` under its own
-- controller. Summoning sickness is meaningless off the battlefield and nothing
-- reads it there; the value keeps those objects out of the sick branch rather
-- than asserting anything.
data Sickness
  = Sick
  | Settled PlayerId
  deriving (Eq, Ord, Show)

module Pawl.Types.TurnScope where

-- CR 102.1 / 109.5: whose turn a phase-scoped ability means. "At the beginning
-- of EACH end step" is EachTurn; "at the beginning of YOUR upkeep" is
-- ControllersTurn, relative to the ability's CONTROLLER, never the card's owner.
-- CR 102.1 is what makes this an axis at all -- "The active player is the player
-- whose turn it is" -- since CR 500.1's phases and steps say nothing about it.
--
-- CR 109.5 is what makes "your" mean the controller, and it answers separately
-- per ability kind. That is why this type names no player at all and each READER
-- supplies its own: for a TRIGGERED ability CR 109.5 gives "the controller of the
-- object when the ability triggered" (CR 603.3a says the same), which
-- Pawl.Engine.Event reads off the trigger; for an ACTIVATED ability it gives "the
-- player who activated the ability", which Pawl.Engine.Activate.timingOk already
-- has in hand as the CR 602.2 activator.
--
-- Shared by Pawl.Types.TriggerCondition's StepBegins (CR 603.2b) and
-- Pawl.Types.ActivationTiming's DuringPhase (CR 307.5). Sharing the SCOPE is not
-- sharing either of those types, which describe different things.
--
-- A CR 603.7 delayed ability keyed to "the NEXT end step" is EachTurn: any
-- player's end step qualifies, and its once-ness comes from the delayed store
-- (CR 603.7b), never from the scope.
data TurnScope
  = EachTurn
  | ControllersTurn
  deriving (Eq, Ord, Show)

module Pawl.Types.TurnScope where

-- | CR 102.1 / 109.5: whose turn a phase-scoped ability means. "At the beginning
-- of EACH end step" is EachTurn; "at the beginning of YOUR upkeep" is
-- ControllersTurn, relative to the ability's CONTROLLER, never the card's owner.
--
-- CR 109.5 makes "your" mean the controller, and answers separately per ability
-- kind. That is why this type names no player and each READER supplies its own:
-- for a triggered ability CR 109.5 and CR 603.3a give the controller when the
-- ability triggered; for an activated one, the CR 602.2 activator.
--
-- Shared by Pawl.Types.TriggerCondition's StepBegins (CR 603.2b) and
-- Pawl.Types.ActivationRestriction's DuringPhase (CR 307.5).
--
-- A CR 603.7 delayed ability keyed to "the NEXT end step" is EachTurn: any
-- player's end step qualifies, and its once-ness comes from the delayed store
-- (CR 603.7b), never from the scope.
data TurnScope
  = EachTurn
  | ControllersTurn
  deriving (Eq, Ord, Show)

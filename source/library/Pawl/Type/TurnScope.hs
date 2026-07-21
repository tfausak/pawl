module Pawl.Type.TurnScope where

-- CR 603.2b / 603.3a: whose turn a step-beginning trigger watches. "At the
-- beginning of EACH end step" is EachTurn; "at the beginning of YOUR upkeep" is
-- ControllersTurn, relative to the ability's CONTROLLER (CR 603.3a), never the
-- card's owner.
--
-- A CR 603.7 delayed ability keyed to "the NEXT end step" is EachTurn: any
-- player's end step qualifies, and its once-ness comes from the delayed store
-- (CR 603.7b), never from the scope.
data TurnScope
  = EachTurn
  | ControllersTurn
  deriving (Eq, Ord, Show)

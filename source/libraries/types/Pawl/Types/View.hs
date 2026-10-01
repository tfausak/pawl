module Pawl.Types.View where

-- | A rendering of part of the game state that a View check compares.
data View
  = -- | CR 405.1: the stack, top first.
    Stack
  | -- | CR 500.1: the current step.
    Step
  | -- | CR 117.1: the actions a player is offered at priority.
    Offered
  | -- | CR 301.5 / 303.4: what an object is attached to.
    AttachedTo
  | -- | CR 108.4: an object's controller.
    Controller
  | -- | CR 104: how the game ended, if it has.
    Result
  | -- | CR 102.1: the active player.
    ActivePlayer
  | -- | CR 117.1: who holds priority.
    Priority
  | -- | CR 400.1: the objects in one of a player's zones, in order.
    Zone
  | -- | CR 105.2: an object's colors.
    Colors
  deriving (Bounded, Enum, Eq, Ord, Show)

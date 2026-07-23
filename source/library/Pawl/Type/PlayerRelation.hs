module Pawl.Type.PlayerRelation where

-- Who an object's controller is, relative to the perspective the evaluation
-- carries (the source's controller when targeting; the effect's controller for a
-- continuous effect). CR 109.5 fixes "you" as the object's controller and, by
-- negation, "an opponent" as any other player still in the game.
data PlayerRelation
  = You
  | Opponent
  deriving (Eq, Ord, Show)

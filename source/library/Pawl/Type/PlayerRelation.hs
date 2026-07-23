module Pawl.Type.PlayerRelation where

-- Who an object's controller is, relative to the perspective the evaluation
-- carries (the source's controller when targeting; the effect's controller for a
-- continuous effect). CR 109.5 fixes "you" as the object's controller; CR 102.2
-- fixes "an opponent" as the other player in a two-player game (the pool's
-- assumption today).
data PlayerRelation
  = You
  | Opponent
  deriving (Eq, Ord, Show)

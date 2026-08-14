module Pawl.Types.PlayerRelation where

-- | Who an object's controller is, relative to the perspective the evaluation
-- carries (the source's controller when targeting; the effect's controller for a
-- continuous effect). CR 109.5 fixes "you" as the object's controller; Opponent
-- is every player who is not the perspective -- CR 806.1 in a free-for-all, CR
-- 102.2 in a two-player game, the same predicate either way. CR 102.3's teams
-- are the ONE reading it is wrong for, and pawl has none to express (#175).
-- Resolved at Pawl.Engine.Count.playersFor and Pawl.Engine.Filter.matches.
data PlayerRelation
  = You
  | Opponent
  deriving (Bounded, Enum, Eq, Ord, Show)

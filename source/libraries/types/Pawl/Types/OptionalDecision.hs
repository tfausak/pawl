module Pawl.Types.OptionalDecision where

-- | CR 603.5 / 608.2d: a player's answer to a printed "may" as the spell or
-- ability resolves. CR 603.5's own wording supplies both names -- an ability
-- goes on the stack "regardless of whether their controller intends to exercise
-- the ability's option or not".
--
-- A named sum rather than a Bool, the posture Concession (Continues/Concedes)
-- and MulliganDecision (Keep/Mulligan) take: every player-facing yes-or-no in
-- this engine is written out, so a transcript reads as the decision it records
-- rather than as an unlabelled boolean.
data OptionalDecision
  = Declines
  | Exercises
  deriving (Eq, Ord, Show)

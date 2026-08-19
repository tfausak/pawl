module Pawl.Types.OptionalDecision where

-- | CR 603.5 / 608.2d: a player's answer to a printed "may" as the spell or
-- ability resolves. CR 603.5's own wording supplies both names.
--
-- A named sum rather than a Bool, the posture every player-facing yes-or-no in
-- this engine takes, so a transcript reads as the decision it records.
data OptionalDecision
  = Declines
  | Exercises
  deriving (Eq, Ord, Show)

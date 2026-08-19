module Pawl.Types.EntwineDecision where

-- | CR 702.42a: a player's answer to entwine's offer of all modes for an
-- additional cost. One decision, not two: the rule states the widened mode choice
-- and the extra payment in a single sentence, so a player who entwines has
-- announced both halves of CR 601.2b at once.
--
-- A named sum rather than a Bool, the posture every player-facing yes-or-no in
-- this engine takes, so a transcript reads as the decision it records.
--
-- Its own type rather than a reuse of OptionalDecision, which is scoped to
-- CR 603.5's printed "may" answered AS THE SPELL RESOLVES. This one is answered
-- while the spell is being CAST (CR 601.2b).
data EntwineDecision
  = Declines
  | Entwines
  deriving (Eq, Ord, Show)

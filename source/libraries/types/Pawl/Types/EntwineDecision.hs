module Pawl.Types.EntwineDecision where

-- CR 702.42a: a player's answer to "You may choose all modes of this spell
-- instead of just the number specified. If you do, you pay an additional
-- [cost]." One decision, not two: rule 702.42a states the widened mode choice
-- and the extra payment in a single sentence, so a player who entwines has
-- announced both halves of CR 601.2b at once.
--
-- A named sum rather than a Bool, the posture Concession (Continues/Concedes),
-- MulliganDecision (Keep/Mulligan) and OptionalDecision (Declines/Exercises)
-- take: every player-facing yes-or-no in this engine is written out, so a
-- transcript reads as the decision it records rather than as an unlabelled
-- boolean.
--
-- Its own type rather than a reuse of OptionalDecision, whose own comment scopes
-- it to CR 603.5's printed "may" answered AS THE SPELL RESOLVES. This one is
-- answered while the spell is being CAST (CR 601.2b), which is a different
-- moment and a different question.
data EntwineDecision
  = Declines
  | Entwines
  deriving (Eq, Ord, Show)

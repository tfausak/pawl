module Pawl.Types.KickerDecision where

-- | CR 702.33a: a player's answer to kicker's offer of an additional cost as they
-- cast a spell. CR 702.33d is what hangs off the answer -- "if a spell's
-- controller declares the intention to pay any of that spell's kicker costs, that
-- spell has been 'kicked'" -- so this is the declaration itself and not a report
-- of a payment that has happened.
--
-- A named sum rather than a Bool, the posture every player-facing yes-or-no in
-- this engine takes, so a transcript reads as the decision it records.
--
-- Its own type rather than a reuse of Pawl.Types.EntwineDecision, whose rule
-- bundles a MODE choice into the same sentence, or of OptionalDecision, which is
-- scoped to CR 603.5's printed "may" answered AS THE SPELL RESOLVES. This one is
-- answered while the spell is being CAST (CR 601.2b) and widens nothing.
--
-- ONE answer, not a count: CR 702.33c's multikicker is payable "any number of
-- times", which is a different question and a different type (#1234).
data KickerDecision
  = Declines
  | Kicks
  deriving (Eq, Ord, Show)

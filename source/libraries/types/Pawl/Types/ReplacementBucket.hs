module Pawl.Types.ReplacementBucket where

-- CR 616.1a-e: the five ordered buckets the affected player picks from. The
-- HIGHEST non-empty bucket wins, and Ord here is ascending in the CR's own order,
-- so "highest non-empty" is the minimum present.
--
-- CopyOnEntry (616.1c) has a producer: Pawl.Replacement's bucketOf sends
-- EntryR AsCopy there, splitting it from EntryR ChoiceOf, which stays in
-- Other along with every other arm -- clone.json is the card that produces an
-- AsCopy rewrite. The remaining three are classification with a documented
-- absence, not machinery pretending to exist: SelfReplacement is CR 614.15
-- self-replacement effects (#68), ControlOnEntry is CR 616.1b's "enters under
-- your control instead" (#69), and BackFaceOnEntry is CR 616.1d, which needs
-- transform (CR 701.27) first (#70).
data ReplacementBucket
  = SelfReplacement -- CR 616.1a
  | ControlOnEntry -- CR 616.1b
  | CopyOnEntry -- CR 616.1c
  | BackFaceOnEntry -- CR 616.1d
  | Other -- CR 616.1e
  deriving (Eq, Ord, Show)

module Pawl.Type.ReplacementBucket where

-- CR 616.1a-e: the five ordered buckets the affected player picks from. The
-- HIGHEST non-empty bucket wins, and Ord here is ascending in the CR's own order,
-- so "highest non-empty" is the minimum present.
--
-- Only CopyOnEntry (616.1c) and Other (616.1e) have producers. The other three
-- are classification with a documented absence, not machinery pretending to
-- exist: SelfReplacement is CR 614.15 self-replacement effects (#N),
-- ControlOnEntry is CR 616.1b's "enters under your control instead" (#N), and
-- BackFaceOnEntry is CR 616.1d, which needs transform (CR 701.27) first (#N).
data ReplacementBucket
  = SelfReplacement -- CR 616.1a
  | ControlOnEntry -- CR 616.1b
  | CopyOnEntry -- CR 616.1c
  | BackFaceOnEntry -- CR 616.1d
  | Other -- CR 616.1e
  deriving (Eq, Ord, Show)

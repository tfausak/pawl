module Pawl.Types.ReplacementBucket where

-- | CR 616.1a-e: the five ordered buckets the affected player picks from. The
-- HIGHEST non-empty bucket wins, and Ord here is ascending in the CR's own order,
-- so "highest non-empty" is the minimum present.
--
-- Every bucket has a producer. SelfReplacement (616.1a) takes candidates by
-- their ORIGIN rather than their payload (Pawl.Types.ReplacementOrigin); Galvanic
-- Blast creates one. ControlOnEntry (616.1b) is EntryR UnderSourceControl, from
-- Gather Specimens. CopyOnEntry (616.1c) is EntryR AsCopy, from Clone, split from
-- EntryR ChoiceOf, which stays in Other with every other arm. BackFaceOnEntry
-- (616.1d) is EntryR EntersTransformed, from CR 712.13a -- daybound's "if it is
-- night ... it enters transformed" (CR 702.145b). Not CR 712.14a's "put onto the
-- battlefield transformed", which is an instruction the effect carries
-- (Pawl.Types.EntryRiders) rather than a replacement effect, and so takes no
-- bucket at all.
data ReplacementBucket
  = SelfReplacement -- CR 616.1a
  | ControlOnEntry -- CR 616.1b
  | CopyOnEntry -- CR 616.1c
  | BackFaceOnEntry -- CR 616.1d
  | Other -- CR 616.1e
  deriving (Eq, Ord, Show)

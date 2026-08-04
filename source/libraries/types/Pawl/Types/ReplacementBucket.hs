module Pawl.Types.ReplacementBucket where

-- | CR 616.1a-e: the five ordered buckets the affected player picks from. The
-- HIGHEST non-empty bucket wins, and Ord here is ascending in the CR's own order,
-- so "highest non-empty" is the minimum present.
--
-- Three buckets have producers. SelfReplacement (616.1a) takes candidates by
-- their ORIGIN rather than their payload (Pawl.Types.ReplacementOrigin); Galvanic
-- Blast creates one. ControlOnEntry (616.1b) is EntryR UnderSourceControl, from
-- Gather Specimens. CopyOnEntry (616.1c) is EntryR AsCopy, from Clone, split from
-- EntryR ChoiceOf, which stays in Other with every other arm. BackFaceOnEntry
-- (616.1d) is classification with a documented absence: it needs transform
-- (CR 701.27) first (#70).
data ReplacementBucket
  = SelfReplacement -- CR 616.1a
  | ControlOnEntry -- CR 616.1b
  | CopyOnEntry -- CR 616.1c
  | BackFaceOnEntry -- CR 616.1d
  | Other -- CR 616.1e
  deriving (Eq, Ord, Show)

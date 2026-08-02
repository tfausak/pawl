module Pawl.Types.ReplacementBucket where

-- | CR 616.1a-e: the five ordered buckets the affected player picks from. The
-- HIGHEST non-empty bucket wins, and Ord here is ascending in the CR's own order,
-- so "highest non-empty" is the minimum present.
--
-- Three buckets have producers. SelfReplacement (616.1a) is CR 614.15's
-- self-replacement effects, and Pawl.Engine.Replacement's bucketOf sends a
-- candidate there by its ORIGIN rather than by its payload (see
-- Pawl.Types.ReplacementOrigin) -- galvanic-blast.json is the card that creates
-- one. ControlOnEntry (616.1b) is EntryR UnderSourceControl --
-- gather-specimens.json produces it, and it is the one bucket whose ORDERING
-- against a lower one is exercised, since a Gather Specimens racing an entering
-- Clone changes who is asked to pick the copy. CopyOnEntry (616.1c) is EntryR
-- AsCopy, split from EntryR ChoiceOf, which stays in Other along with every
-- other arm -- clone.json produces it. The remaining one is classification with
-- a documented absence, not machinery pretending to exist: BackFaceOnEntry is CR
-- 616.1d, which needs transform (CR 701.27) first (#70).
data ReplacementBucket
  = SelfReplacement -- CR 616.1a
  | ControlOnEntry -- CR 616.1b
  | CopyOnEntry -- CR 616.1c
  | BackFaceOnEntry -- CR 616.1d
  | Other -- CR 616.1e
  deriving (Eq, Ord, Show)

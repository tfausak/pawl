module Pawl.Types.CandidateId where

import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.Timestamp as Timestamp

-- | CR 614.5's identity: what counts as ONE replacement effect for the rule that
-- an effect gets only one opportunity to affect an event. Without it Hardened
-- Scales and Corpsejack Menace re-fire on each other's output forever -- a
-- termination condition, not an optimization.
--
-- A permanent's static replacement ability is identified by (source, effect
-- VALUE), NOT (source, list index). Index identity would break CR 616.2: a Clone
-- applies its own `EntryR AsCopy` at index 0, replacing its copiable snapshot
-- with a Primal Plasma's, whose `EntryR (ChoiceOf ...)` is then also index 0, so
-- the newly-acquired ability would be mistaken for the one already used.
--
-- Two Doubling Seasons are still two opportunities: different SOURCES. The cost
-- of value identity is that a single source carrying two TEXTUALLY IDENTICAL
-- replacement abilities gets one opportunity instead of two; no card in the pool
-- does that (#75).
--
-- A floating replacement is identified by (source, timestamp): GameState's
-- timestamp counter is monotone, so two Fogs are two instances even from one
-- source object.
data CandidateId
  = OfPermanent ObjectId.ObjectId ReplacementEffect.ReplacementEffect
  | OfFloating ObjectId.ObjectId Timestamp.Timestamp
  deriving (Eq, Ord, Show)

module Pawl.Types.CandidateId where

import Numeric.Natural (Natural)
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.Timestamp as Timestamp

-- | CR 614.5's identity: what counts as ONE replacement effect for the rule that
-- an effect gets only one opportunity to affect an event. Without it Hardened
-- Scales and Corpsejack Menace re-fire on each other's output forever -- a
-- termination condition, not an optimization.
--
-- A permanent's static replacement ability is identified by (source, effect
-- VALUE, ordinal among EQUALS), NOT (source, list index). Index identity would
-- break CR 616.2: a Clone applies its own `EntryR AsCopy` at index 0, replacing
-- its copiable snapshot with a Primal Plasma's, whose `EntryR (ChoiceOf ...)` is
-- then also index 0, so the newly-acquired ability would be mistaken for the one
-- already used.
--
-- The ordinal is what CR 702.136b needs -- "if a permanent has multiple
-- instances of riot, each works separately" -- and it counts only rows EQUAL in
-- (source, effect), so it is immune to the shuffle that sinks a list index: the
-- Clone above acquires and loses whole abilities between iterations without ever
-- renumbering a surviving one. Pawl.Engine.Replacement.collect assigns it.
--
-- Two Doubling Seasons are still two opportunities: different SOURCES.
--
-- A floating replacement is identified by (source, timestamp): GameState's
-- timestamp counter is monotone, so two Fogs are two instances even from one
-- source object.
data CandidateId
  = OfPermanent ObjectId.ObjectId (ReplacementEffect.ReplacementEffect (Effect.Effect Card.Card)) Natural
  | OfFloating ObjectId.ObjectId Timestamp.Timestamp
  deriving (Eq, Ord, Show)

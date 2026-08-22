module Pawl.Types.PermanentCandidate where

import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.InstanceOrdinal as InstanceOrdinal
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect

-- | CR 614.5's identity for a permanent's STATIC replacement ability: the
-- source, the effect VALUE, and the ordinal among equals. Pawl.Types.CandidateId
-- carries the rule and the reasoning; this is the payload that arm names.
--
-- A record rather than three positional fields, so which of the two ids is the
-- source cannot be got wrong at a construction site, and so the arm has the one
-- payload a codec needs.
data PermanentCandidate = MkPermanentCandidate
  { source :: ObjectId.ObjectId,
    effect :: ReplacementEffect.ReplacementEffect (Effect.Effect Card.Card),
    ordinal :: InstanceOrdinal.InstanceOrdinal
  }
  deriving (Eq, Ord, Show)

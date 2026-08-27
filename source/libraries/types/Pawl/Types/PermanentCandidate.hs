module Pawl.Types.PermanentCandidate where

import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.InstanceOrdinal as InstanceOrdinal
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.ReplacementProvenance as ReplacementProvenance

-- | CR 614.5's identity for a permanent's STATIC replacement ability: the
-- source, where the row came from, the effect VALUE, and the ordinal among
-- equals. Pawl.Types.CandidateId carries the rule and the reasoning; this is the
-- payload that arm names.
--
-- A record rather than four positional fields, so which of the two ids is the
-- source cannot be got wrong at a construction site, and so the arm has the one
-- payload a codec needs.
--
-- The provenance is part of the IDENTITY and not a note beside it, which is the
-- rule rather than a convenience: a card printing exactly the row rule 702.16e
-- mints for the protection it also has would otherwise collapse the two into one
-- instance, where CR 614.5 gives each its own opportunity.
data PermanentCandidate = MkPermanentCandidate
  { source :: ObjectId.ObjectId,
    provenance :: ReplacementProvenance.ReplacementProvenance,
    effect :: ReplacementEffect.ReplacementEffect (Effect.Effect Card.Card (GrantedAbility.GrantedAbility Card.Card)),
    ordinal :: InstanceOrdinal.InstanceOrdinal
  }
  deriving (Eq, Ord, Show)

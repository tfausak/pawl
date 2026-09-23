module Pawl.Types.ActiveCopy where

import qualified Data.Set as Set
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.Types.Timestamp as Timestamp

-- | CR 613.1a / 611.2: a stored, resolution-generated COPY EFFECT with a stated
-- duration, held in GameState.copyEffects. Mirrorweave's "each other creature
-- becomes a copy of target nonlegendary creature until end of turn" is the
-- producer.
--
-- Read at Pawl.Engine.Projection.View.stampedSnapshotOf, which layers a live row
-- over the copy stamp underneath it -- so every one of the seven readers that
-- accessor serves sees the copy, and CR 707.3's "objects that copy the object
-- will use the new copiable values" keeps holding for a copy taken while the row
-- stands. What the row does that a stamp cannot is END: `expiry` decides when a
-- Pawl.Engine.Expiry sweep drops it (CR 514.2, 611.2a, 611.2b), and the read
-- falls back to the stamp -- delete-and-recompute, so nothing is explicitly
-- undone (design.md 2.5).
--
-- ITS OWN CARRIER rather than a GameState.continuousEffects row, and the module
-- graph is what decides it: the payload is a whole ProjectedCharacteristics, and
-- Pawl.Types.Modification cannot name that type -- the cycle runs Modification ->
-- ProjectedCharacteristics -> Card -> Face -> StaticAbility -> Modification. So
-- this joins the Active* carriers (ActiveUnregeneratable and its siblings), which
-- is how pawl already stores every continuous effect whose payload the
-- Modification vocabulary does not hold. CR 613 is unbothered: layer 1a is read
-- as the layer fold's SEED (Pawl.Engine.Projection.copiableCharacteristics)
-- rather than gathered with layers 2-7, so a copy effect has no Modification to
-- be in either carrier.
--
-- A bare id SET where the printed carrier holds an ObjectRef, for
-- ActiveUnregeneratable's reason and CR 611.2c's: the ref is swept ONCE, as the
-- ability resolves, and the objects it named are what the effect covers
-- thereafter.
--
-- `snapshot` is the copiable values CR 707.2 read off the original, exceptions
-- already folded in (CR 707.9a) -- the same value the no-duration road stamps
-- into Binding.copyOf, so the two roads cannot disagree about what a copy is.
--
-- `timestamp` orders two live rows over one object (CR 613.7): the latest one is
-- what layer 1a leaves, since a copy effect REPLACES copiable values rather than
-- adding to them. A stamp made later drops its object from `objects`
-- (Pawl.Engine.Game.supersedeStoredCopies), which is how a row is ordered
-- against a stamp that carries no timestamp -- proved by Pawl.CopySpec's "CR
-- 613.7 a copy effect made after Mirrorweave's outranks it (Dimir Doppelganger)".
data ActiveCopy = MkActiveCopy
  { source :: ObjectId.ObjectId,
    timestamp :: Timestamp.Timestamp,
    expiry :: Expiry.Expiry,
    -- | The objects that became copies.
    objects :: Set.Set ObjectId.ObjectId,
    -- | CR 707.2: the copiable values they took on.
    snapshot :: ProjectedCharacteristics.ProjectedCharacteristics
  }
  deriving (Eq, Ord, Show)

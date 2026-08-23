module Pawl.Codec.ActiveBlockRequirementSpec where

import qualified Pawl.Codec.ActiveBlockRequirement as ActiveBlockRequirement
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ActiveBlockRequirement as ActiveBlockRequirement
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.Timestamp as Timestamp

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ActiveBlockRequirement" $ do
  -- CR 509.1c, provoke's row (CR 702.39a). All three ObjectIds differ, so no two
  -- of them can be swapped silently, and provoke's "this combat" arms the phase
  -- expiry rather than CR 514.2's cleanup.
  Spec.it s "a creature that must block an attacker" $
    Common.assertCodec
      s
      ActiveBlockRequirement.codec
      ActiveBlockRequirement.MkActiveBlockRequirement
        { ActiveBlockRequirement.source = ObjectId.MkObjectId 1,
          ActiveBlockRequirement.timestamp = Timestamp.MkTimestamp 2,
          ActiveBlockRequirement.expiry = Expiry.AtEndOf PhaseSelector.CombatPhase,
          ActiveBlockRequirement.blocker = ObjectId.MkObjectId 3,
          ActiveBlockRequirement.attacker = ObjectId.MkObjectId 4
        }
      " {\"source\":1,\"timestamp\":2,\"expiry\":{\"type\":\"AtEndOf\",\"value\":{\"type\":\"CombatPhase\"}},\"blocker\":3,\"attacker\":4} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ActiveBlockRequirement.codec

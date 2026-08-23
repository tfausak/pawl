module Pawl.Codec.ActiveAttackRequirementSpec where

import qualified Pawl.Codec.ActiveAttackRequirement as ActiveAttackRequirement
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ActiveAttackRequirement as ActiveAttackRequirement
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Timestamp as Timestamp

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ActiveAttackRequirement" $ do
  -- CR 508.1d, Alluring Siren's row. `source` and `attacker` are both ObjectIds
  -- and hold different ids, so the two cannot be swapped silently; CR 514.2's
  -- AtCleanup is what "this turn" arms.
  Spec.it s "a creature that must attack a player" $
    Common.assertCodec
      s
      ActiveAttackRequirement.codec
      ActiveAttackRequirement.MkActiveAttackRequirement
        { ActiveAttackRequirement.source = ObjectId.MkObjectId 1,
          ActiveAttackRequirement.timestamp = Timestamp.MkTimestamp 2,
          ActiveAttackRequirement.expiry = Expiry.AtCleanup,
          ActiveAttackRequirement.attacker = ObjectId.MkObjectId 3,
          ActiveAttackRequirement.defender = PlayerId.MkPlayerId 4
        }
      " {\"source\":1,\"timestamp\":2,\"expiry\":{\"type\":\"AtCleanup\"},\"attacker\":3,\"defender\":4} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ActiveAttackRequirement.codec

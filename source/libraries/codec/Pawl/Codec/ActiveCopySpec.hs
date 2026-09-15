module Pawl.Codec.ActiveCopySpec where

import qualified Data.Set as Set
import qualified Pawl.Codec.ActiveCopy as ActiveCopy
import qualified Pawl.Codec.ProjectedCharacteristicsSpec as ProjectedCharacteristicsSpec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ActiveCopy as ActiveCopy
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Timestamp as Timestamp

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ActiveCopy" $ do
  -- Mirrorweave's row, over two subjects. `source` and the two subjects are all
  -- distinct ObjectIds, so a swap cannot pass; "until end of turn" arms CR
  -- 514.2's AtCleanup. The snapshot is the all-default fixture next door, which
  -- keeps this spec about the row's own keys.
  Spec.it s "two permanents copying one original until end of turn" $
    Common.assertCodec
      s
      ActiveCopy.codec
      ActiveCopy.MkActiveCopy
        { ActiveCopy.source = ObjectId.MkObjectId 1,
          ActiveCopy.timestamp = Timestamp.MkTimestamp 2,
          ActiveCopy.expiry = Expiry.AtCleanup,
          ActiveCopy.objects = Set.fromList [ObjectId.MkObjectId 3, ObjectId.MkObjectId 4],
          ActiveCopy.snapshot = ProjectedCharacteristicsSpec.minimalCharacteristics
        }
      " {\"source\":1,\"timestamp\":2,\"expiry\":{\"type\":\"AtCleanup\"},\"objects\":[3,4],\"snapshot\":{\"names\":[\"Mountain\"],\"cardTypes\":[{\"type\":\"Land\"}]}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ActiveCopy.codec

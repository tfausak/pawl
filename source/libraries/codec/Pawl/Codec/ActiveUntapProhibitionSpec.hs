module Pawl.Codec.ActiveUntapProhibitionSpec where

import qualified Pawl.Codec.ActiveUntapProhibition as ActiveUntapProhibition
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ActiveUntapProhibition as ActiveUntapProhibition
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Timestamp as Timestamp

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ActiveUntapProhibition" $ do
  -- CR 502.3. `source` and `object` are both ObjectIds and differ, so a swap
  -- cannot pass.
  Spec.it s "a permanent that does not untap" $
    Common.assertCodec
      s
      ActiveUntapProhibition.codec
      ActiveUntapProhibition.MkActiveUntapProhibition
        { ActiveUntapProhibition.source = ObjectId.MkObjectId 1,
          ActiveUntapProhibition.timestamp = Timestamp.MkTimestamp 2,
          ActiveUntapProhibition.expiry = Expiry.AtCleanup,
          ActiveUntapProhibition.object = ObjectId.MkObjectId 3
        }
      " {\"source\":1,\"timestamp\":2,\"expiry\":{\"type\":\"AtCleanup\"},\"object\":3} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ActiveUntapProhibition.codec

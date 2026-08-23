module Pawl.Codec.ActiveUnregeneratableSpec where

import qualified Pawl.Codec.ActiveUnregeneratable as ActiveUnregeneratable
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ActiveUnregeneratable as ActiveUnregeneratable
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Timestamp as Timestamp

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ActiveUnregeneratable" $ do
  -- CR 701.19c, Hurr Jackal's row. `source` and `object` are both ObjectIds and
  -- differ, so a swap cannot pass; "this turn" arms CR 514.2's AtCleanup.
  Spec.it s "a permanent that can't be regenerated this turn" $
    Common.assertCodec
      s
      ActiveUnregeneratable.codec
      ActiveUnregeneratable.MkActiveUnregeneratable
        { ActiveUnregeneratable.source = ObjectId.MkObjectId 1,
          ActiveUnregeneratable.timestamp = Timestamp.MkTimestamp 2,
          ActiveUnregeneratable.expiry = Expiry.AtCleanup,
          ActiveUnregeneratable.object = ObjectId.MkObjectId 3
        }
      " {\"source\":1,\"timestamp\":2,\"expiry\":{\"type\":\"AtCleanup\"},\"object\":3} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ActiveUnregeneratable.codec

module Pawl.Codec.ActiveBlockProhibitionSpec where

import qualified Pawl.Codec.ActiveBlockProhibition as ActiveBlockProhibition
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ActiveBlockProhibition as ActiveBlockProhibition
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Timestamp as Timestamp

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ActiveBlockProhibition" $ do
  -- CR 509.1b, Zirda's row. `source` and `object` are both ObjectIds and differ,
  -- so a swap cannot pass; "this turn" arms CR 514.2's AtCleanup.
  Spec.it s "a permanent that can't block this turn" $
    Common.assertCodec
      s
      ActiveBlockProhibition.codec
      ActiveBlockProhibition.MkActiveBlockProhibition
        { ActiveBlockProhibition.source = ObjectId.MkObjectId 1,
          ActiveBlockProhibition.timestamp = Timestamp.MkTimestamp 2,
          ActiveBlockProhibition.expiry = Expiry.AtCleanup,
          ActiveBlockProhibition.object = ObjectId.MkObjectId 3
        }
      " {\"source\":1,\"timestamp\":2,\"expiry\":{\"type\":\"AtCleanup\"},\"object\":3} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ActiveBlockProhibition.codec

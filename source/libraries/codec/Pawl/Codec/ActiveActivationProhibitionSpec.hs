module Pawl.Codec.ActiveActivationProhibitionSpec where

import qualified Pawl.Codec.ActiveActivationProhibition as ActiveActivationProhibition
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ActiveActivationProhibition as ActiveActivationProhibition
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Timestamp as Timestamp

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ActiveActivationProhibition" $ do
  -- CR 602.2, Deadlock Trap's row. `source` and `object` are both ObjectIds and
  -- differ, so a swap cannot pass; "this turn" arms CR 514.2's AtCleanup.
  Spec.it s "a permanent whose activated abilities can't be activated this turn" $
    Common.assertCodec
      s
      ActiveActivationProhibition.codec
      ActiveActivationProhibition.MkActiveActivationProhibition
        { ActiveActivationProhibition.source = ObjectId.MkObjectId 1,
          ActiveActivationProhibition.timestamp = Timestamp.MkTimestamp 2,
          ActiveActivationProhibition.expiry = Expiry.AtCleanup,
          ActiveActivationProhibition.object = ObjectId.MkObjectId 3
        }
      " {\"source\":1,\"timestamp\":2,\"expiry\":{\"type\":\"AtCleanup\"},\"object\":3} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ActiveActivationProhibition.codec

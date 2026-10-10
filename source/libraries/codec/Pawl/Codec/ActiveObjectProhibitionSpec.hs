module Pawl.Codec.ActiveObjectProhibitionSpec where

import qualified Pawl.Codec.ActiveObjectProhibition as ActiveObjectProhibition
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ActiveObjectProhibition as ActiveObjectProhibition
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Prohibition as Prohibition
import qualified Pawl.Types.Timestamp as Timestamp

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ActiveObjectProhibition" $ do
  -- CR 602.5. `source` and `object` are both ObjectIds and differ, so a swap
  -- cannot pass.
  Spec.it s "a permanent whose abilities can't be activated" $
    Common.assertCodec
      s
      ActiveObjectProhibition.codec
      ActiveObjectProhibition.MkActiveObjectProhibition
        { ActiveObjectProhibition.source = ObjectId.MkObjectId 1,
          ActiveObjectProhibition.timestamp = Timestamp.MkTimestamp 2,
          ActiveObjectProhibition.expiry = Expiry.AtCleanup,
          ActiveObjectProhibition.what = Prohibition.Activate,
          ActiveObjectProhibition.object = ObjectId.MkObjectId 3
        }
      " {\"source\":1,\"timestamp\":2,\"expiry\":{\"type\":\"AtCleanup\"},\"what\":{\"type\":\"Activate\"},\"object\":3} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ActiveObjectProhibition.codec

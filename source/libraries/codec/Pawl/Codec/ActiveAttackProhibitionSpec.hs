module Pawl.Codec.ActiveAttackProhibitionSpec where

import qualified Pawl.Codec.ActiveAttackProhibition as ActiveAttackProhibition
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ActiveAttackProhibition as ActiveAttackProhibition
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Timestamp as Timestamp

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ActiveAttackProhibition" $ do
  -- CR 508.1c, Netter en-Dal's row. `source` and `object` are both ObjectIds
  -- and differ, so a swap cannot pass; "this turn" arms CR 514.2's AtCleanup.
  Spec.it s "a permanent that can't attack this turn" $
    Common.assertCodec
      s
      ActiveAttackProhibition.codec
      ActiveAttackProhibition.MkActiveAttackProhibition
        { ActiveAttackProhibition.source = ObjectId.MkObjectId 1,
          ActiveAttackProhibition.timestamp = Timestamp.MkTimestamp 2,
          ActiveAttackProhibition.expiry = Expiry.AtCleanup,
          ActiveAttackProhibition.object = ObjectId.MkObjectId 3
        }
      " {\"source\":1,\"timestamp\":2,\"expiry\":{\"type\":\"AtCleanup\"},\"object\":3} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ActiveAttackProhibition.codec

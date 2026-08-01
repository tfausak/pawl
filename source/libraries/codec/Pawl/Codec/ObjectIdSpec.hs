module Pawl.Codec.ObjectIdSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ObjectId as ObjectId

spec :: (Monad m) => Spec.Spec m n -> n ()
spec s =
  Spec.describe s "Pawl.Codec.ObjectId" . Spec.it s "MkObjectId" $
    Common.assertJsonCodec s ObjectId.toJson ObjectId.fromJson (ObjectId.MkObjectId 7) "7"

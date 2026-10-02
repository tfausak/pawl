module Pawl.Codec.RoomHalfSpec where

import qualified Pawl.Codec.RoomHalf as RoomHalf
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.RoomHalf as RoomHalf

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.RoomHalf" $ do
  Spec.it s "LeftHalf" $
    Common.assertCodec
      s
      RoomHalf.codec
      RoomHalf.LeftHalf
      " {\"type\":\"LeftHalf\"} "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s RoomHalf.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s RoomHalf.codec

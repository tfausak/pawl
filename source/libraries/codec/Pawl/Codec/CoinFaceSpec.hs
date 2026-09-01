module Pawl.Codec.CoinFaceSpec where

import qualified Pawl.Codec.CoinFace as CoinFace
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CoinFace as CoinFace

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CoinFace" $ do
  Spec.it s "Heads" $
    Common.assertCodec
      s
      CoinFace.codec
      CoinFace.Heads
      " {\"type\":\"Heads\"} "
  Spec.it s "Tails" $
    Common.assertCodec
      s
      CoinFace.codec
      CoinFace.Tails
      " {\"type\":\"Tails\"} "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s CoinFace.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s CoinFace.codec

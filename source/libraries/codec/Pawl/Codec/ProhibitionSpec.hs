module Pawl.Codec.ProhibitionSpec where

import qualified Pawl.Codec.Prohibition as Prohibition
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Prohibition as Prohibition

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Prohibition" $ do
  Spec.it s "Regenerate" $
    Common.assertCodec
      s
      Prohibition.codec
      Prohibition.Regenerate
      " {\"type\":\"Regenerate\"} "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s Prohibition.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s Prohibition.codec

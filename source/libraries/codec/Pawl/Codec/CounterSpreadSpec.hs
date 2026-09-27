module Pawl.Codec.CounterSpreadSpec where

import qualified Pawl.Codec.CounterSpread as CounterSpread
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CounterSpread as CounterSpread

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CounterSpread" $ do
  Spec.it s "FromOne" $
    Common.assertCodec
      s
      CounterSpread.codec
      CounterSpread.FromOne
      " {\"type\":\"FromOne\"} "

  Spec.it s "FromAmong" $
    Common.assertCodec
      s
      CounterSpread.codec
      CounterSpread.FromAmong
      " {\"type\":\"FromAmong\"} "

  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s CounterSpread.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s CounterSpread.codec

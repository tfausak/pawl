module Pawl.Codec.DiceReadingSpec where

import qualified Pawl.Codec.DiceReading as DiceReading
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DiceReading as DiceReading

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DiceReading" $ do
  Spec.it s "ChooseOne" $
    Common.assertCodec
      s
      DiceReading.codec
      DiceReading.ChooseOne
      " {\"type\":\"ChooseOne\"} "
  Spec.it s "Total" $
    Common.assertCodec
      s
      DiceReading.codec
      DiceReading.Total
      " {\"type\":\"Total\"} "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s DiceReading.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s DiceReading.codec

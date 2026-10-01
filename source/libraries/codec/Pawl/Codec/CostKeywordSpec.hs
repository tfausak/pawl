module Pawl.Codec.CostKeywordSpec where

import qualified Pawl.Codec.CostKeyword as CostKeyword
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CostKeyword as CostKeyword

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CostKeyword" $ do
  Spec.it s "Scavenge" $
    Common.assertCodec
      s
      CostKeyword.codec
      CostKeyword.Scavenge
      " {\"type\":\"Scavenge\"} "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s CostKeyword.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s CostKeyword.codec

module Pawl.Codec.ProductionTagSpec where

import qualified Pawl.Codec.ProductionTag as ProductionTag
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ProductionTag as ProductionTag

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ProductionTag" $ do
  Spec.it s "Snow" $
    Common.assertCodec
      s
      ProductionTag.codec
      ProductionTag.Snow
      " {\"type\":\"Snow\"} "
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s ProductionTag.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s ProductionTag.codec

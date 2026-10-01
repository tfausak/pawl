module Pawl.Codec.GraveyardOrderSpec where

import qualified Pawl.Codec.GraveyardOrder as GraveyardOrder
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.GraveyardOrder as GraveyardOrder.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.GraveyardOrder" $ do
  Spec.it s "Matters" $
    Common.assertCodec s GraveyardOrder.codec GraveyardOrder.Type.Matters " {\"type\":\"Matters\"} "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s GraveyardOrder.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s GraveyardOrder.codec

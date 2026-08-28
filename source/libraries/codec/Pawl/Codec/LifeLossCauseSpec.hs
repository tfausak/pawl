module Pawl.Codec.LifeLossCauseSpec where

import qualified Pawl.Codec.LifeLossCause as LifeLossCause
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.LifeLossCause as LifeLossCause

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.LifeLossCause" $ do
  -- CR 120.4c.
  Spec.it s "ByDamage" $
    Common.assertCodec s LifeLossCause.codec LifeLossCause.ByDamage " {\"type\":\"ByDamage\"} "
  -- CR 119.3.
  Spec.it s "ByEffect" $
    Common.assertCodec s LifeLossCause.codec LifeLossCause.ByEffect " {\"type\":\"ByEffect\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s LifeLossCause.codec

module Pawl.Codec.ZonePairSpec where

import qualified Pawl.Codec.ZonePair as ZonePair
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ZonePair as ZonePair

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ZonePair" $ do
  Spec.it s "HandAndGraveyard" $
    Common.assertCodec
      s
      ZonePair.codec
      ZonePair.HandAndGraveyard
      " {\"type\":\"HandAndGraveyard\"} "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s ZonePair.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s ZonePair.codec

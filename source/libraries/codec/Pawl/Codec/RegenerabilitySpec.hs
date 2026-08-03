module Pawl.Codec.RegenerabilitySpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Regenerability as Regenerability
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Regenerability as Regenerability

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Regenerability" $ do
  Spec.it s "Regenerable" $
    Common.assertJsonCodec
      s
      Regenerability.toJson
      Regenerability.fromJson
      Regenerability.Regenerable
      "{\"type\":\"Regenerable\"}"
  Spec.it s "CantBeRegenerated" $
    Common.assertJsonCodec
      s
      Regenerability.toJson
      Regenerability.fromJson
      Regenerability.CantBeRegenerated
      "{\"type\":\"CantBeRegenerated\"}"

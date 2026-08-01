module Pawl.Codec.ExtraPhaseSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ExtraPhase as ExtraPhase
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ExtraPhase as ExtraPhase

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ExtraPhase" $ do
  Spec.it s "ExtraCombat" $
    Common.assertJsonCodec s ExtraPhase.toJson ExtraPhase.fromJson ExtraPhase.ExtraCombat "{\"type\":\"ExtraCombat\"}"
  Spec.it s "ExtraMain" $
    Common.assertJsonCodec s ExtraPhase.toJson ExtraPhase.fromJson ExtraPhase.ExtraMain "{\"type\":\"ExtraMain\"}"

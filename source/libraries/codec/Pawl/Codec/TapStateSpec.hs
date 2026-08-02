module Pawl.Codec.TapStateSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.TapState as TapState
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.TapState as TapState

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TapState" $ do
  Spec.it s "Untapped" $
    Common.assertJsonCodec s TapState.toJson TapState.fromJson TapState.Untapped "{\"type\":\"Untapped\"}"
  Spec.it s "Tapped" $
    Common.assertJsonCodec s TapState.toJson TapState.fromJson TapState.Tapped "{\"type\":\"Tapped\"}"

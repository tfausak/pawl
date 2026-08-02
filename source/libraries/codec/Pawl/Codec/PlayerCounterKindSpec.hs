module Pawl.Codec.PlayerCounterKindSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlayerCounterKind" $ do
  Spec.it s "Energy" $
    Common.assertJsonCodec s PlayerCounterKind.toJson PlayerCounterKind.fromJson PlayerCounterKind.Energy "{\"type\":\"Energy\"}"
  Spec.it s "Poison" $
    Common.assertJsonCodec s PlayerCounterKind.toJson PlayerCounterKind.fromJson PlayerCounterKind.Poison "{\"type\":\"Poison\"}"

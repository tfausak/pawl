module Pawl.Codec.ReplacementOriginSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ReplacementOrigin" $ do
  Spec.it s "SelfReplacement" $
    Common.assertJsonCodec s ReplacementOrigin.toJson ReplacementOrigin.fromJson ReplacementOrigin.SelfReplacement "{\"type\":\"SelfReplacement\"}"
  Spec.it s "Other" $
    Common.assertJsonCodec s ReplacementOrigin.toJson ReplacementOrigin.fromJson ReplacementOrigin.Other "{\"type\":\"Other\"}"

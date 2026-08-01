module Pawl.Codec.MonarchTargetSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.MonarchTarget as MonarchTarget
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.MonarchTarget as MonarchTarget

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.MonarchTarget" $ do
  Spec.it s "TheController" $
    Common.assertJsonCodec s MonarchTarget.toJson MonarchTarget.fromJson MonarchTarget.TheController "{\"type\":\"TheController\"}"
  Spec.it s "ControllerOfSource" $
    Common.assertJsonCodec s MonarchTarget.toJson MonarchTarget.fromJson MonarchTarget.ControllerOfSource "{\"type\":\"ControllerOfSource\"}"

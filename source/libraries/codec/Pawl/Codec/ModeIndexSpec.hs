module Pawl.Codec.ModeIndexSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ModeIndex as ModeIndex
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ModeIndex as ModeIndex

spec :: (Monad m) => Spec.Spec m n -> n ()
spec s =
  Spec.describe s "Pawl.Codec.ModeIndex" . Spec.it s "MkModeIndex" $
    Common.assertJsonCodec
      s
      ModeIndex.toJson
      ModeIndex.fromJson
      (ModeIndex.MkModeIndex 2)
      "2"

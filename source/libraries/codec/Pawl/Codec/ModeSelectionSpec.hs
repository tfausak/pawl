{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ModeSelectionSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ModeSelection as ModeSelection
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ModeSelection as ModeSelection

spec :: (Monad m) => Spec.Spec m n -> n ()
spec s =
  Spec.describe s "Pawl.Codec.ModeSelection" . Spec.it s "ChooseExactly" $
    Common.assertJsonCodec
      s
      ModeSelection.toJson
      ModeSelection.fromJson
      (ModeSelection.ChooseExactly 1)
      """ {"type":"ChooseExactly","value":1} """

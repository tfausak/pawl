{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ManaTypeSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ManaType as ManaType
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.ManaType as ManaType

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ManaType" $ do
  Spec.it s "Colored" $
    Common.assertJsonCodec
      s
      ManaType.toJson
      ManaType.fromJson
      (ManaType.Colored Color.Red)
      """ {"type":"Colored","value":{"type":"Red"}} """
  Spec.it s "Colorless" $
    Common.assertJsonCodec
      s
      ManaType.toJson
      ManaType.fromJson
      ManaType.Colorless
      """ {"type":"Colorless"} """

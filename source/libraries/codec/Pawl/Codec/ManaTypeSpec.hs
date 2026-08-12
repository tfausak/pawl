{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ManaTypeSpec where

import qualified Pawl.Codec.ManaType as ManaType
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.ManaType as ManaType

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ManaType" $ do
  Spec.it s "Colored" $
    Common.assertCodec
      s
      ManaType.codec
      (ManaType.Colored Color.Red)
      """ {"type":"Colored","value":{"type":"Red"}} """
  Spec.it s "Colorless" $
    Common.assertCodec
      s
      ManaType.codec
      ManaType.Colorless
      """ {"type":"Colorless"} """
  Spec.it s "has a schema" $
    Common.assertHasSchema s ManaType.codec

{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.RoundingSpec where

import qualified Pawl.Codec.Rounding as Rounding
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Rounding as Rounding

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Rounding" $ do
  Spec.it s "Up" $
    Common.assertCodec
      s
      Rounding.codec
      Rounding.Up
      """ {"type":"Up"} """
  Spec.it s "Down" $
    Common.assertCodec
      s
      Rounding.codec
      Rounding.Down
      """ {"type":"Down"} """
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s Rounding.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s Rounding.codec

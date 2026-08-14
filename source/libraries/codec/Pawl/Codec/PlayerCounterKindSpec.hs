{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.PlayerCounterKindSpec where

import qualified Pawl.Codec.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlayerCounterKind" $ do
  Spec.it s "Energy" $
    Common.assertCodec
      s
      PlayerCounterKind.codec
      PlayerCounterKind.Energy
      """ {"type":"Energy"} """
  Spec.it s "Poison" $
    Common.assertCodec
      s
      PlayerCounterKind.codec
      PlayerCounterKind.Poison
      """ {"type":"Poison"} """
  Spec.it s "Rad" $
    Common.assertCodec
      s
      PlayerCounterKind.codec
      PlayerCounterKind.Rad
      """ {"type":"Rad"} """
  Spec.it s "Experience" $
    Common.assertCodec
      s
      PlayerCounterKind.codec
      PlayerCounterKind.Experience
      """ {"type":"Experience"} """
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s PlayerCounterKind.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s PlayerCounterKind.codec

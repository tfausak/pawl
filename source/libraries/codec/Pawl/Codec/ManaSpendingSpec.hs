{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ManaSpendingSpec where

import qualified Pawl.Codec.ManaSpending as ManaSpending
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ManaSpending as ManaSpending

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ManaSpending" $ do
  Spec.it s "AsProduced" $
    Common.assertCodec
      s
      ManaSpending.codec
      ManaSpending.AsProduced
      """ {"type":"AsProduced"} """
  Spec.it s "AnyType" $
    Common.assertCodec
      s
      ManaSpending.codec
      ManaSpending.AnyType
      """ {"type":"AnyType"} """
  -- Exhaustive where the literals above are representative: Arm.enum derives the
  -- arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s ManaSpending.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s ManaSpending.codec

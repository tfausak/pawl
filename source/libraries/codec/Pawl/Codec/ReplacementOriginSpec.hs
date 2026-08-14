{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ReplacementOriginSpec where

import qualified Pawl.Codec.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ReplacementOrigin" $ do
  Spec.it s "SelfReplacement" $
    Common.assertCodec
      s
      ReplacementOrigin.codec
      ReplacementOrigin.SelfReplacement
      """ {"type":"SelfReplacement"} """
  Spec.it s "Other" $
    Common.assertCodec
      s
      ReplacementOrigin.codec
      ReplacementOrigin.Other
      """ {"type":"Other"} """
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s ReplacementOrigin.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s ReplacementOrigin.codec

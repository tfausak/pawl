{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.OptionalitySpec where

import qualified Pawl.Codec.Optionality as Optionality
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Optionality as Optionality

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Optionality" $ do
  Spec.it s "Mandatory" $
    Common.assertCodec
      s
      Optionality.codec
      Optionality.Mandatory
      """ {"type":"Mandatory"} """
  Spec.it s "Optional" $
    Common.assertCodec
      s
      Optionality.codec
      Optionality.Optional
      """ {"type":"Optional"} """
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s Optionality.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s Optionality.codec

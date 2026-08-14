{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.DesignationSpec where

import qualified Pawl.Codec.Designation as Designation
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Designation as Designation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Designation" $ do
  Spec.it s "Renowned" $
    Common.assertCodec
      s
      Designation.codec
      Designation.Renowned
      """ {"type":"Renowned"} """
  Spec.it s "Monstrous" $
    Common.assertCodec
      s
      Designation.codec
      Designation.Monstrous
      """ {"type":"Monstrous"} """
  Spec.it s "Suspected" $
    Common.assertCodec
      s
      Designation.codec
      Designation.Suspected
      """ {"type":"Suspected"} """
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s Designation.codec

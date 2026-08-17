{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.PayObligationSpec where

import qualified Pawl.Codec.PayObligation as PayObligation
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PayObligation as PayObligation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PayObligation" $ do
  -- CR 118.12a's limb, which is every card in the pool but Standstill.
  Spec.it s "Optional" $
    Common.assertCodec
      s
      PayObligation.codec
      PayObligation.Optional
      """ {"type":"Optional"} """
  -- CR 118.12's other limb, Standstill's.
  Spec.it s "Mandatory" $
    Common.assertCodec
      s
      PayObligation.codec
      PayObligation.Mandatory
      """ {"type":"Mandatory"} """
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s PayObligation.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s PayObligation.codec

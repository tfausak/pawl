module Pawl.Codec.RequiredDefenderSpec where

import qualified Pawl.Codec.RequiredDefender as RequiredDefender
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.RequiredDefender as RequiredDefender

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.RequiredDefender" $ do
  Spec.it s "ControllerOfAttached" $
    Common.assertCodec
      s
      RequiredDefender.codec
      RequiredDefender.ControllerOfAttached
      " {\"type\":\"ControllerOfAttached\"} "
  Spec.it s "OpponentWithMostLife" $
    Common.assertCodec
      s
      RequiredDefender.codec
      RequiredDefender.OpponentWithMostLife
      " {\"type\":\"OpponentWithMostLife\"} "
  -- Exhaustive where the literal above is representative: Arm.enum derives the
  -- arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s RequiredDefender.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s RequiredDefender.codec

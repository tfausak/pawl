module Pawl.Codec.OnsetSpec where

import qualified Pawl.Codec.Onset as Onset
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Onset as Onset

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Onset" $ do
  Spec.it s "Immediately" $
    Common.assertCodec
      s
      Onset.codec
      Onset.Immediately
      " {\"type\":\"Immediately\"} "
  Spec.it s "FromYourNextTurn" $
    Common.assertCodec
      s
      Onset.codec
      Onset.FromYourNextTurn
      " {\"type\":\"FromYourNextTurn\"} "
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s Onset.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s Onset.codec

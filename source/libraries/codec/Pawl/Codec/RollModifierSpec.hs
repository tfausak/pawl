module Pawl.Codec.RollModifierSpec where

import qualified Pawl.Codec.RollModifier as RollModifier
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.RollModifier as RollModifier

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.RollModifier" $ do
  -- CR 706.2b's first step: Clam-I-Am.
  Spec.it s "Reroll" $
    Common.assertCodec
      s
      RollModifier.codec
      RollModifier.Reroll
      " {\"type\":\"Reroll\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s RollModifier.codec

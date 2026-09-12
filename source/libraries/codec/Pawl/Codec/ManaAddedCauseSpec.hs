module Pawl.Codec.ManaAddedCauseSpec where

import qualified Pawl.Codec.ManaAddedCause as ManaAddedCause
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ManaAddedCause as ManaAddedCause

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ManaAddedCause" $ do
  Spec.it s "ManaAbility" $
    Common.assertCodec
      s
      ManaAddedCause.codec
      ManaAddedCause.ManaAbility
      " {\"type\":\"ManaAbility\"} "
  Spec.it s "Resolution" $
    Common.assertCodec
      s
      ManaAddedCause.codec
      ManaAddedCause.Resolution
      " {\"type\":\"Resolution\"} "
  -- Pawl.Codec.RevealCauseSpec's reason: Arm.enum derives the arm list, so this
  -- is what would catch a constructor the derivation missed.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s ManaAddedCause.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s ManaAddedCause.codec

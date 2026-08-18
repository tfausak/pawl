module Pawl.Codec.ManaRetentionSpec where

import qualified Pawl.Codec.ManaRetention as ManaRetention
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ManaRetention as ManaRetention

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ManaRetention" $ do
  Spec.it s "Ordinary" $
    Common.assertCodec
      s
      ManaRetention.codec
      ManaRetention.Ordinary
      " {\"type\":\"Ordinary\"} "
  Spec.it s "UntilEndOfTurn" $
    Common.assertCodec
      s
      ManaRetention.codec
      ManaRetention.UntilEndOfTurn
      " {\"type\":\"UntilEndOfTurn\"} "
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s ManaRetention.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s ManaRetention.codec

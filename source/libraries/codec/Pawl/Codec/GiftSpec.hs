module Pawl.Codec.GiftSpec where

import qualified Pawl.Codec.Gift as Gift
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Gift as Gift

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Gift" $ do
  Spec.it s "Card" $
    Common.assertCodec
      s
      Gift.codec
      Gift.Card
      " {\"type\":\"Card\"} "
  -- Exhaustive where the literal above is representative, Pawl.Codec.DiscardCause's
  -- reason: Arm.enum derives the arm list from the type.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s Gift.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s Gift.codec

module Pawl.Codec.TimesSpec where

import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.Times as Times
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Times as Times

-- | Instantiated at 'Quantity.Quantity', Pawl.Codec.HalvedSpec's shape and for
-- its reason: 'Pawl.Codec.Quantity' passes its own recursive codec in.
codec :: Codec.Codec (Times.Times Quantity.Quantity)
codec = Times.codec Quantity.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Times" $ do
  -- CR 107.1: Blessed Reversal's 3 times a count. The inner value is a whole Quantity, so the recursion has to survive the trip.
  Spec.it s "MkTimes" $
    Common.assertCodec
      s
      codec
      ( Times.MkTimes
          { Times.factor = 3,
            Times.quantity = Quantity.Power
          }
      )
      " {\"factor\":3,\"quantity\":{\"type\":\"Power\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec

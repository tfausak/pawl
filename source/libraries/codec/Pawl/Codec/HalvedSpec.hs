{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.HalvedSpec where

import qualified Pawl.Codec.Halved as Halved
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Halved as Halved
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Rounding as Rounding

-- | Instantiated at 'Quantity.Quantity', the only concrete instantiation
-- anywhere: 'Pawl.Codec.Quantity' passes its own recursive codec in.
codec :: Codec.Codec (Halved.Halved Quantity.Quantity)
codec = Halved.codec Quantity.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Halved" $ do
  -- CR 107.1a: Malignus' half, rounded up. The inner value is a whole Quantity, so the recursion has to survive the trip.
  Spec.it s "MkHalved" $
    Common.assertCodec
      s
      codec
      ( Halved.MkHalved
          { Halved.rounding = Rounding.Up,
            Halved.quantity = Quantity.Power
          }
      )
      """ {"rounding":{"type":"Up"},"quantity":{"type":"Power"}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s codec

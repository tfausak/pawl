module Pawl.Codec.DepartureSpec where

import qualified Pawl.Codec.Departure as Departure
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Departure as Departure

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Departure" $ do
  Spec.it s "Lost" $
    Common.assertCodec
      s
      Departure.codec
      Departure.Lost
      " {\"type\":\"Lost\"} "
  Spec.it s "Conceded" $
    Common.assertCodec
      s
      Departure.codec
      Departure.Conceded
      " {\"type\":\"Conceded\"} "
  Spec.it s "Drew" $
    Common.assertCodec
      s
      Departure.codec
      Departure.Drew
      " {\"type\":\"Drew\"} "
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s Departure.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s Departure.codec

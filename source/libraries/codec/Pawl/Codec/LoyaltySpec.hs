module Pawl.Codec.LoyaltySpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Loyalty as Loyalty
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Loyalty as Loyalty

spec :: (Monad m) => Spec.Spec m n -> n ()
spec s =
  Spec.describe s "Pawl.Codec.Loyalty" . Spec.it s "MkLoyalty" $
    Common.assertJsonCodec s Loyalty.toJson Loyalty.fromJson (Loyalty.MkLoyalty 3) "3"

module Pawl.Codec.CounterabilitySpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Counterability as Counterability
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Counterability as Counterability

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Counterability" $ do
  Spec.it s "Counterable" $
    Common.assertJsonCodec s Counterability.toJson Counterability.fromJson Counterability.Counterable "{\"type\":\"Counterable\"}"
  Spec.it s "CantBeCountered" $
    Common.assertJsonCodec s Counterability.toJson Counterability.fromJson Counterability.CantBeCountered "{\"type\":\"CantBeCountered\"}"
  Spec.describe s "fromJsonDefault" $ do
    -- CR 113.6g is printed text, so an absent key means Counterable.
    Spec.it s "absent key decodes as Counterable" $
      Common.assertFromJson s Counterability.fromJsonDefault "null" Counterability.Counterable
    Spec.it s "CantBeCountered still decodes explicitly" $
      Common.assertFromJson s Counterability.fromJsonDefault "{\"type\":\"CantBeCountered\"}" Counterability.CantBeCountered

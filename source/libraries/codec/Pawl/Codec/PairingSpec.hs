module Pawl.Codec.PairingSpec where

import qualified Pawl.Codec.Pairing as Pairing
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Pairing as Pairing
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Pairing" $ do
  -- CR 702.95b's pairing. The two numbers differ so that a codec which read the
  -- partner out of the seat, or the other way round, cannot pass.
  Spec.it s "MkPairing, both keys" $
    Common.assertCodec
      s
      Pairing.codec
      ( Pairing.MkPairing
          { Pairing.partner = ObjectId.MkObjectId 7,
            Pairing.under = PlayerId.MkPlayerId 3
          }
      )
      " {\"partner\":7,\"under\":3} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Pairing.codec

module Pawl.Codec.LandPlayedSpec where

import qualified Pawl.Codec.LandPlayed as LandPlayed
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.LandPlayed as LandPlayed
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.LandPlayed" $ do
  -- CR 305.1: a land played out of exile.
  Spec.it s "MkLandPlayed, every key" $
    Common.assertCodec
      s
      LandPlayed.codec
      ( LandPlayed.MkLandPlayed
          { LandPlayed.player = PlayerId.MkPlayerId 1,
            LandPlayed.land = ObjectId.MkObjectId 7,
            LandPlayed.from = Zone.Exile
          }
      )
      " {\"player\":1,\"land\":7,\"from\":{\"type\":\"Exile\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s LandPlayed.codec

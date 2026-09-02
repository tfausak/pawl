module Pawl.Codec.PermanentWasSacrificedSpec where

import qualified Pawl.Codec.PermanentWasSacrificed as PermanentWasSacrificed
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PermanentWasSacrificed as PermanentWasSacrificed
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PermanentWasSacrificed" $ do
  -- CR 701.21a.
  Spec.it s "MkPermanentWasSacrificed, both keys" $
    Common.assertCodec
      s
      PermanentWasSacrificed.codec
      ( PermanentWasSacrificed.MkPermanentWasSacrificed
          { PermanentWasSacrificed.player = PlayerId.MkPlayerId 0,
            PermanentWasSacrificed.permanent = ObjectId.MkObjectId 1
          }
      )
      " {\"player\":0,\"permanent\":1} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PermanentWasSacrificed.codec

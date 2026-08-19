module Pawl.Codec.PermanentSacrificedSpec where

import qualified Pawl.Codec.PermanentSacrificed as PermanentSacrificed
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PermanentSacrificed as PermanentSacrificed
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PermanentSacrificed" $ do
  -- CR 701.21a.
  Spec.it s "MkPermanentSacrificed, both keys" $
    Common.assertCodec
      s
      PermanentSacrificed.codec
      ( PermanentSacrificed.MkPermanentSacrificed
          { PermanentSacrificed.player = PlayerId.MkPlayerId 0,
            PermanentSacrificed.permanent = ObjectId.MkObjectId 1
          }
      )
      " {\"player\":0,\"permanent\":1} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PermanentSacrificed.codec

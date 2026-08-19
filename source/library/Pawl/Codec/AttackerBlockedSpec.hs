module Pawl.Codec.AttackerBlockedSpec where

import qualified Pawl.Codec.AttackerBlocked as AttackerBlocked
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AttackerBlocked as AttackerBlocked
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AttackerBlocked" $ do
  -- CR 509.1h, carrying CR 508.5's defending player.
  Spec.it s "MkAttackerBlocked, both keys" $
    Common.assertCodec
      s
      AttackerBlocked.codec
      ( AttackerBlocked.MkAttackerBlocked
          { AttackerBlocked.attacker = ObjectId.MkObjectId 1,
            AttackerBlocked.defender = PlayerId.MkPlayerId 1
          }
      )
      " {\"attacker\":1,\"defender\":1} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AttackerBlocked.codec

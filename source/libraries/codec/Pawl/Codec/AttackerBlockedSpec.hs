module Pawl.Codec.AttackerBlockedSpec where

import qualified Pawl.Codec.AttackerBlocked as AttackerBlocked
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AttackerBlocked as AttackerBlocked
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AttackerBlocked" $ do
  -- CR 509.1h's escape clause, where an effect makes a creature blocked by
  -- nobody: the count is the field's default, so neither key is written.
  Spec.it s "MkAttackerBlocked, blocked by no creatures at all" $
    Common.assertCodec
      s
      AttackerBlocked.codec
      ( AttackerBlocked.MkAttackerBlocked
          { AttackerBlocked.attacker = ObjectId.MkObjectId 1,
            AttackerBlocked.defender = PlayerId.MkPlayerId 1,
            AttackerBlocked.blockers = 0
          }
      )
      " {\"attacker\":1,\"defender\":1} "
  -- CR 509.1h's ordinary road, carrying CR 508.5's defending player and CR
  -- 509.3e's count. Every value differs from every other, which is what catches
  -- a codec that read one key into another field.
  Spec.it s "MkAttackerBlocked, all three keys" $
    Common.assertCodec
      s
      AttackerBlocked.codec
      ( AttackerBlocked.MkAttackerBlocked
          { AttackerBlocked.attacker = ObjectId.MkObjectId 1,
            AttackerBlocked.defender = PlayerId.MkPlayerId 2,
            AttackerBlocked.blockers = 3
          }
      )
      " {\"attacker\":1,\"defender\":2,\"blockers\":3} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AttackerBlocked.codec

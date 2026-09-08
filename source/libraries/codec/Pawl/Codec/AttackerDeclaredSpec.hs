module Pawl.Codec.AttackerDeclaredSpec where

import qualified Pawl.Codec.AttackerDeclared as AttackerDeclared
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.AttackerDeclared as AttackerDeclared
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AttackerDeclared" $ do
  -- CR 508.1, carrying CR 508.1b's announcement and CR 702.83b's declaration
  -- size. The battle attacked is not the defending player's own id, which is CR
  -- 310.9d's substitution and the reason both fields are on the wire.
  Spec.it s "MkAttackerDeclared, every key" $
    Common.assertCodec
      s
      AttackerDeclared.codec
      ( AttackerDeclared.MkAttackerDeclared
          { AttackerDeclared.attacker = ObjectId.MkObjectId 1,
            AttackerDeclared.defender = PlayerId.MkPlayerId 1,
            AttackerDeclared.target = AttackTarget.OfBattle (ObjectId.MkObjectId 3),
            AttackerDeclared.count = 2
          }
      )
      " {\"attacker\":1,\"defender\":1,\"target\":{\"type\":\"OfBattle\",\"value\":3},\"count\":2} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AttackerDeclared.codec

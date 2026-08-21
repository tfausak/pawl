module Pawl.Codec.AttackTargetSpec where

import qualified Pawl.Codec.AttackTarget as AttackTarget
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AttackTarget" $ do
  Spec.it s "OfPlayer" $
    Common.assertCodec
      s
      AttackTarget.codec
      (AttackTarget.OfPlayer (PlayerId.MkPlayerId 1))
      " {\"type\":\"OfPlayer\",\"value\":1} "
  -- CR 306.6 and CR 310.5 name a PERMANENT where the arm above names a player, so
  -- the two carry different id types and each gets a case.
  Spec.it s "OfPlaneswalker" $
    Common.assertCodec
      s
      AttackTarget.codec
      (AttackTarget.OfPlaneswalker (ObjectId.MkObjectId 2))
      " {\"type\":\"OfPlaneswalker\",\"value\":2} "
  Spec.it s "OfBattle" $
    Common.assertCodec
      s
      AttackTarget.codec
      (AttackTarget.OfBattle (ObjectId.MkObjectId 3))
      " {\"type\":\"OfBattle\",\"value\":3} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AttackTarget.codec

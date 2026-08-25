module Pawl.Codec.BecameAttackedSpec where

import qualified Pawl.Codec.BecameAttacked as BecameAttacked
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.BecameAttacked as BecameAttacked
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.BecameAttacked" $ do
  -- CR 508.3e's pair: who declared, and what they sent creatures at.
  Spec.it s "MkBecameAttacked, every key" $
    Common.assertCodec
      s
      BecameAttacked.codec
      ( BecameAttacked.MkBecameAttacked
          { BecameAttacked.attacker = PlayerId.MkPlayerId 1,
            BecameAttacked.target = AttackTarget.OfPlayer (PlayerId.MkPlayerId 2)
          }
      )
      " {\"attacker\":1,\"target\":{\"type\":\"OfPlayer\",\"value\":2}} "
  -- CR 508.1b's other targets ride the same field, which is why CR 508.3e's arm
  -- has to narrow to OfPlayer rather than take any BecameAttacked it sees.
  Spec.it s "a planeswalker target" $
    Common.assertCodec
      s
      BecameAttacked.codec
      ( BecameAttacked.MkBecameAttacked
          { BecameAttacked.attacker = PlayerId.MkPlayerId 3,
            BecameAttacked.target = AttackTarget.OfPlaneswalker (ObjectId.MkObjectId 6)
          }
      )
      " {\"attacker\":3,\"target\":{\"type\":\"OfPlaneswalker\",\"value\":6}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s BecameAttacked.codec

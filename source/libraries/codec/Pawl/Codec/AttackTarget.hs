module Pawl.Codec.AttackTarget where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.AttackTarget as AttackTarget

-- | CR 508.1b's three announcements. Reached from card data only through
-- Pawl.Codec.GameEvent's BecameAttacked arm; the combat record reaches it the
-- other way, through Pawl.Codec.Combat (#126).
codec :: Codec.Codec AttackTarget.AttackTarget
codec =
  Arm.tagged
    [ Arm.payload "OfPlayer" PlayerId.codec AttackTarget.OfPlayer (\x -> case x of AttackTarget.OfPlayer y -> Just y; _ -> Nothing),
      Arm.payload "OfPlaneswalker" ObjectId.codec AttackTarget.OfPlaneswalker (\x -> case x of AttackTarget.OfPlaneswalker y -> Just y; _ -> Nothing),
      Arm.payload "OfBattle" ObjectId.codec AttackTarget.OfBattle (\x -> case x of AttackTarget.OfBattle y -> Just y; _ -> Nothing)
    ]

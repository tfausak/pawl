module Pawl.Codec.PermanentDealsCombatDamageToPlayerSpec where

import qualified Pawl.Codec.PermanentDealsCombatDamageToPlayer as PermanentDealsCombatDamageToPlayer
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PermanentDealsCombatDamageToPlayer as PermanentDealsCombatDamageToPlayer
import qualified Pawl.Types.PlayerRelation as PlayerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PermanentDealsCombatDamageToPlayer" $ do
  -- The two keys name different relations, so a codec that crossed them would
  -- not round-trip.
  Spec.it s "MkPermanentDealsCombatDamageToPlayer, both keys" $
    Common.assertCodec
      s
      PermanentDealsCombatDamageToPlayer.codec
      ( PermanentDealsCombatDamageToPlayer.MkPermanentDealsCombatDamageToPlayer
          { PermanentDealsCombatDamageToPlayer.filter = Filter.ControlledBy PlayerRelation.Opponent,
            PermanentDealsCombatDamageToPlayer.recipient = PlayerRelation.You
          }
      )
      " {\"filter\":{\"type\":\"ControlledBy\",\"value\":{\"type\":\"Opponent\"}},\"recipient\":{\"type\":\"You\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PermanentDealsCombatDamageToPlayer.codec

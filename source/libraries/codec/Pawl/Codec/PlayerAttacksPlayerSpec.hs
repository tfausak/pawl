module Pawl.Codec.PlayerAttacksPlayerSpec where

import qualified Pawl.Codec.PlayerAttacksPlayer as PlayerAttacksPlayer
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PlayerAttacksPlayer as PlayerAttacksPlayer
import qualified Pawl.Types.PlayerRelation as PlayerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlayerAttacksPlayer" $ do
  -- Lulu, Stern Guardian's payload: who declared, and whom they declared
  -- against. The two keys hold DIFFERENT relations here, so a codec that
  -- crossed them would not round-trip.
  Spec.it s "MkPlayerAttacksPlayer, both keys" $
    Common.assertCodec
      s
      PlayerAttacksPlayer.codec
      ( PlayerAttacksPlayer.MkPlayerAttacksPlayer
          { PlayerAttacksPlayer.attacker = PlayerRelation.Opponent,
            PlayerAttacksPlayer.attacked = PlayerRelation.You
          }
      )
      " {\"attacker\":{\"type\":\"Opponent\"},\"attacked\":{\"type\":\"You\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PlayerAttacksPlayer.codec

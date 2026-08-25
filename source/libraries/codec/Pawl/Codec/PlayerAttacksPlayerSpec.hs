module Pawl.Codec.PlayerAttacksPlayerSpec where

import qualified Pawl.Codec.PlayerAttacksPlayer as PlayerAttacksPlayer
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PlayerAttacksPlayer as PlayerAttacksPlayer
import qualified Pawl.Types.PlayerRelation as PlayerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlayerAttacksPlayer" $ do
  -- Seifer, Balamb Rival's side of CR 508.3e.
  Spec.it s "MkPlayerAttacksPlayer, both keys" $
    Common.assertCodec
      s
      PlayerAttacksPlayer.codec
      ( PlayerAttacksPlayer.MkPlayerAttacksPlayer
          { PlayerAttacksPlayer.attacker = PlayerRelation.You,
            PlayerAttacksPlayer.attacked = PlayerRelation.AnyPlayer
          }
      )
      " {\"attacker\":{\"type\":\"You\"},\"attacked\":{\"type\":\"AnyPlayer\"}} "
  -- Mirkwood Trapper's, which is the same two relations the other way round --
  -- so a codec that read one key for both would round-trip the case above and
  -- fail this one.
  Spec.it s "the two relations are not interchangeable" $
    Common.assertCodec
      s
      PlayerAttacksPlayer.codec
      ( PlayerAttacksPlayer.MkPlayerAttacksPlayer
          { PlayerAttacksPlayer.attacker = PlayerRelation.AnyPlayer,
            PlayerAttacksPlayer.attacked = PlayerRelation.You
          }
      )
      " {\"attacker\":{\"type\":\"AnyPlayer\"},\"attacked\":{\"type\":\"You\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PlayerAttacksPlayer.codec

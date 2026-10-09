module Pawl.Codec.AimedPlayersSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.AimedPlayers as AimedPlayers
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AimedPlayers as AimedPlayers
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AimedPlayers" $ do
  Spec.it s "Scoped" $
    Common.assertCodec
      s
      AimedPlayers.codec
      (AimedPlayers.Scoped (PlayerScope.Related PlayerRelation.You))
      " {\"type\":\"Scoped\",\"value\":{\"type\":\"You\"}} "
  Spec.it s "EachInSlot" $
    Common.assertCodec
      s
      AimedPlayers.codec
      (AimedPlayers.EachInSlot (SlotName.MkSlotName (Text.pack "highest")))
      " {\"type\":\"EachInSlot\",\"value\":\"highest\"} "
  -- Runtime-only, and round-tripped because the codec is total.
  Spec.it s "BoundPlayer" $
    Common.assertCodec
      s
      AimedPlayers.codec
      (AimedPlayers.BoundPlayer (PlayerId.MkPlayerId 2))
      " {\"type\":\"BoundPlayer\",\"value\":2} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AimedPlayers.codec

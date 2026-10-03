module Pawl.Codec.AimedPlayers where

import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.AimedPlayers as AimedPlayers

-- | Tagged, Pawl.Codec.ZoneScope's shape: a nested PlayerScope, a slot name, or
-- the baked player.
codec :: Codec.Codec AimedPlayers.AimedPlayers
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Scoped" PlayerScope.codec AimedPlayers.Scoped (\x -> case x of AimedPlayers.Scoped y -> Just y; _ -> Nothing),
      Arm.payload "EachInSlot" SlotName.codec AimedPlayers.EachInSlot (\x -> case x of AimedPlayers.EachInSlot y -> Just y; _ -> Nothing),
      Arm.payload "BoundPlayer" PlayerId.codec AimedPlayers.BoundPlayer (\x -> case x of AimedPlayers.BoundPlayer y -> Just y; _ -> Nothing)
    ]

tagOf :: AimedPlayers.AimedPlayers -> String
tagOf x = case x of
  AimedPlayers.Scoped {} -> "Scoped"
  AimedPlayers.EachInSlot {} -> "EachInSlot"
  AimedPlayers.BoundPlayer {} -> "BoundPlayer"

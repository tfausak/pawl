{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.SlotPerPlayer where

import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.SlotPerPlayer as SlotPerPlayer

-- | A bare object keyed by the record's field names, both required: "for each
-- opponent" and "for each player" are different printed sentences.
codec :: Codec.Codec SlotPerPlayer.SlotPerPlayer
codec = Fields.object $ do
  players <- Fields.required "players" PlayerRelation.codec SlotPerPlayer.players
  slot <- Fields.required "slot" SlotName.codec SlotPerPlayer.slot
  pure
    SlotPerPlayer.MkSlotPerPlayer
      { SlotPerPlayer.players = players,
        SlotPerPlayer.slot = slot
      }

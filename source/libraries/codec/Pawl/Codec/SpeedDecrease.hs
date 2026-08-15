{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.SpeedDecrease where

import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.SpeedDecrease as SpeedDecrease

-- | A bare object keyed by the record's field names, Pawl.Codec.Mill's shape.
-- The floor is ELIDED when it is 0, which is what a card printing no such
-- sentence says.
codec :: Codec.Codec SpeedDecrease.SpeedDecrease
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec SpeedDecrease.player
  quantity <- Fields.required "quantity" Quantity.codec SpeedDecrease.quantity
  floor_ <- Fields.defaulted "floor" 0 Common.natural SpeedDecrease.floor
  pure
    SpeedDecrease.MkSpeedDecrease
      { SpeedDecrease.player = player,
        SpeedDecrease.quantity = quantity,
        SpeedDecrease.floor = floor_
      }

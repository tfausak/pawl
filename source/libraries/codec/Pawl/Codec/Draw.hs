{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Draw where

import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Draw as Draw

-- | The slot is ELIDED when absent, as Mill's is.
codec :: Codec.Codec Draw.Draw
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec Draw.player
  quantity <- Fields.required "quantity" Quantity.codec Draw.quantity
  slot <- Fields.defaulted "slot" Nothing (Common.maybe SlotName.codec) Draw.slot
  pure
    Draw.MkDraw
      { Draw.player = player,
        Draw.quantity = quantity,
        Draw.slot = slot
      }

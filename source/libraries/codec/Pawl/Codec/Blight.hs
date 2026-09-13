{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Blight where

import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Blight as Blight

-- | A bare object keyed by the record's field names, the slot ELIDED when
-- absent as Mill's is.
codec :: Codec.Codec Blight.Blight
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec Blight.player
  quantity <- Fields.required "quantity" Quantity.codec Blight.quantity
  slot <- Fields.defaulted "slot" Nothing (Common.maybe SlotName.codec) Blight.slot
  pure
    Blight.MkBlight
      { Blight.player = player,
        Blight.quantity = quantity,
        Blight.slot = slot
      }

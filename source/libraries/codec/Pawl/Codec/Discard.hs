{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Discard where

import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Discard as Discard

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's Discard arm.
codec :: Codec.Codec Discard.Discard
codec = Fields.object $ do
  slot <- Fields.required "slot" SlotName.codec Discard.slot
  quantity <- Fields.required "quantity" Quantity.codec Discard.quantity
  pure
    Discard.MkDiscard
      { Discard.slot = slot,
        Discard.quantity = quantity
      }

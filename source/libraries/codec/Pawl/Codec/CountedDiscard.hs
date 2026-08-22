{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CountedDiscard where

import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CountedDiscard as CountedDiscard

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Discard's Counted arm.
codec :: Codec.Codec CountedDiscard.CountedDiscard
codec = Fields.object $ do
  slot <- Fields.required "slot" SlotName.codec CountedDiscard.slot
  quantity <- Fields.required "quantity" Quantity.codec CountedDiscard.quantity
  pure
    CountedDiscard.MkCountedDiscard
      { CountedDiscard.slot = slot,
        CountedDiscard.quantity = quantity
      }

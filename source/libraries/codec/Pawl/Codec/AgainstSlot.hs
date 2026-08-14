{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AgainstSlot where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AgainstSlot as AgainstSlot

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464).
--
-- The quantity codec is a PARAMETER rather than an import, for the reason
-- Pawl.Types.AgainstSlot gives: the record is parametric in the quantity so that
-- neither module has to name the other. Pawl.Codec.Quantity passes its own
-- 'Pawl.Codec.Quantity.codec' in, which is the knot Pawl.Codec.Count already
-- ties from the same place.
codec :: (Typeable.Typeable quantity) => Codec.Codec quantity -> Codec.Codec (AgainstSlot.AgainstSlot quantity)
codec quantityCodec = Fields.object $ do
  slot <- Fields.required "slot" SlotName.codec AgainstSlot.slot
  quantity <- Fields.required "quantity" quantityCodec AgainstSlot.quantity
  pure AgainstSlot.MkAgainstSlot {AgainstSlot.slot = slot, AgainstSlot.quantity = quantity}

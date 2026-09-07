{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Designate where

import qualified Pawl.Codec.Designation as Designation
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Designate as Designate

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's Designate arm.
codec :: Codec.Codec Designate.Designate
codec = Fields.object $ do
  designation <- Fields.required "designation" Designation.codec Designate.designation
  slot <- Fields.required "slot" SlotName.codec Designate.slot
  value <- Fields.defaulted "value" Nothing (Common.maybe Quantity.codec) Designate.value
  pure
    Designate.MkDesignate
      { Designate.designation = designation,
        Designate.slot = slot,
        Designate.value = value
      }

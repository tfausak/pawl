{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Amass where

import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Amass as Amass

-- | A bare object keyed by the record's field names, the slot ELIDED when absent
-- as Mill's is. The tag that picks it is written by Pawl.Codec.Effect's Amass
-- arm.
codec :: Codec.Codec Amass.Amass
codec = Fields.object $ do
  quantity <- Fields.required "quantity" Quantity.codec Amass.quantity
  subtype <- Fields.required "subtype" Subtype.codec Amass.subtype
  slot <- Fields.defaulted "slot" Nothing (Common.maybe SlotName.codec) Amass.slot
  pure
    Amass.MkAmass
      { Amass.quantity = quantity,
        Amass.subtype = subtype,
        Amass.slot = slot
      }

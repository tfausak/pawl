{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Amass where

import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Amass as Amass

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's Amass arm.
codec :: Codec.Codec Amass.Amass
codec = Fields.object $ do
  quantity <- Fields.required "quantity" Quantity.codec Amass.quantity
  subtype <- Fields.required "subtype" Subtype.codec Amass.subtype
  pure
    Amass.MkAmass
      { Amass.quantity = quantity,
        Amass.subtype = subtype
      }

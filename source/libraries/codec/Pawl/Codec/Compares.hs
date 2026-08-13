module Pawl.Codec.Compares where

import qualified Pawl.Codec.Comparison as Comparison
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Compares as Compares

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Condition's Compares arm.
--
-- Naming the sides is the point: both are a Quantity, so a positional payload
-- would let a card file that swapped them decode into the wrong comparison.
codec :: Codec.Codec Compares.Compares
codec =
  Fields.object $
    Compares.MkCompares
      <$> Fields.required "measured" Quantity.codec Compares.measured
      <*> Fields.required "comparison" Comparison.codec Compares.comparison
      <*> Fields.required "threshold" Quantity.codec Compares.threshold

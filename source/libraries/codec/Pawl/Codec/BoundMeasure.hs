{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.BoundMeasure where

import qualified Pawl.Codec.Measure as Measure
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.BoundMeasure as BoundMeasure

-- | A bare object keyed by the record's field names.
codec :: Codec.Codec BoundMeasure.BoundMeasure
codec = Fields.object $ do
  slot <- Fields.required "slot" SlotName.codec BoundMeasure.slot
  measure <- Fields.required "measure" Measure.codec BoundMeasure.measure
  pure BoundMeasure.MkBoundMeasure {BoundMeasure.slot = slot, BoundMeasure.measure = measure}

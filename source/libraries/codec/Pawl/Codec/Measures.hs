{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Measures where

import qualified Pawl.Codec.Comparison as Comparison
import qualified Pawl.Codec.Measure as Measure
import qualified Pawl.Codec.Operand as Operand
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Measures as Measures

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Filter's Measures arm.
codec :: Codec.Codec Measures.Measures
codec = Fields.object $ do
  measure <- Fields.required "measure" Measure.codec Measures.measure
  comparison <- Fields.required "comparison" Comparison.codec Measures.comparison
  operand <- Fields.required "operand" Operand.codec Measures.operand
  pure
    Measures.MkMeasures
      { Measures.measure = measure,
        Measures.comparison = comparison,
        Measures.operand = operand
      }

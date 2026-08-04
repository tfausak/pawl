{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.AggregationSpec where

import qualified Pawl.Codec.Aggregation as Aggregation
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Aggregation as Aggregation

-- Instantiated at Integer via Common.integer/Common.asInteger, the simplest
-- element codec available -- Aggregation.Aggregation is parametric in the
-- per-member quantity it reads (see the module haddock), and no card cares
-- which concrete element type this spec exercises.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Aggregation" $ do
  Spec.it s "Objects" $
    Common.assertJsonCodec
      s
      (Aggregation.toJson Common.integer)
      (Aggregation.fromJson Common.asInteger)
      Aggregation.Objects
      """ {"type":"Objects"} """
  Spec.it s "DistinctCardTypes" $
    Common.assertJsonCodec
      s
      (Aggregation.toJson Common.integer)
      (Aggregation.fromJson Common.asInteger)
      Aggregation.DistinctCardTypes
      """ {"type":"DistinctCardTypes"} """
  Spec.it s "Greatest" $
    Common.assertJsonCodec
      s
      (Aggregation.toJson Common.integer)
      (Aggregation.fromJson Common.asInteger)
      (Aggregation.Greatest 3)
      """ {"type":"Greatest","value":3} """

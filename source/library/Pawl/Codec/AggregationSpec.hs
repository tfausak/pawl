module Pawl.Codec.AggregationSpec where

import qualified Pawl.Codec.Aggregation as Aggregation
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Aggregation as Aggregation

-- Instantiated at Integer, the simplest element codec available: Aggregation is
-- parametric in the per-member quantity it reads, and no card cares which
-- concrete element type this spec exercises.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Aggregation" $ do
  Spec.it s "Members" $
    Common.assertCodec
      s
      (Aggregation.codec Common.integer)
      Aggregation.Members
      " {\"type\":\"Members\"} "
  Spec.it s "DistinctCardTypes" $
    Common.assertCodec
      s
      (Aggregation.codec Common.integer)
      Aggregation.DistinctCardTypes
      " {\"type\":\"DistinctCardTypes\"} "
  Spec.it s "Greatest" $
    Common.assertCodec
      s
      (Aggregation.codec Common.integer)
      (Aggregation.Greatest 3)
      " {\"type\":\"Greatest\",\"value\":3} "
  Spec.it s "has a schema" $ Common.assertHasSchema s (Aggregation.codec Common.integer)

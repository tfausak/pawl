module Pawl.Codec.Aggregation where

import qualified Data.Typeable as Typeable
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Aggregation as Aggregation

-- | No longer wholly nullary: Greatest carries the per-member quantity it
-- reads. Parametric in that quantity, exactly as 'Aggregation.Aggregation' is,
-- so the codec reaches the payload only through the codec passed in -- which is
-- what lets this module sit below @Pawl.Codec.Quantity@ rather than in a cycle
-- with it.
--
-- Converting this forces 'Pawl.Codec.Count' and 'Pawl.Codec.Quantity' to convert
-- with it: a parametric codec needs a @Codec q@ where the loose pair needed only
-- an encode\/decode function, and that requirement propagates up every
-- parametric caller (#1306).
codec :: (Typeable.Typeable q) => Codec.Codec q -> Codec.Codec (Aggregation.Aggregation q)
codec quantityCodec =
  Arm.tagged
    encode
    [ Arm.nullary "Members" Aggregation.Members,
      Arm.nullary "DistinctCardTypes" Aggregation.DistinctCardTypes,
      Arm.payload "Greatest" quantityCodec Aggregation.Greatest
    ]
  where
    encode a = case a of
      Aggregation.Members -> Common.nullary "Members"
      Aggregation.DistinctCardTypes -> Common.nullary "DistinctCardTypes"
      Aggregation.Greatest q -> Common.tagged "Greatest" . Just $ Codec.encode quantityCodec q

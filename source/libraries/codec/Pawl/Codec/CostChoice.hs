module Pawl.Codec.CostChoice where

import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.CostChoice as CostChoice

-- | A bare ARRAY of costs, Pawl.Codec.ManaCost's shape and for its reason: a
-- named domain type wrapping a collection gets a $defs entry of its own, and the
-- wrapper adds no key a card author would have to write.
--
-- 'Common.nonEmpty' is what rejects the empty array on the wire, which
-- Pawl.Types.CostChoice says is a cost with no way to pay it.
codec :: Codec.Codec CostChoice.CostChoice
codec =
  Common.wrapper
    (Common.nonEmpty (Cost.codec Keyword.codec))
    CostChoice.MkCostChoice
    CostChoice.unwrap

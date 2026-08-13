module Pawl.Codec.Count where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Aggregation as Aggregation
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Scope as Scope
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Count as Count

-- | A BARE OBJECT keyed by the record's field names, the shape every
-- single-constructor record in this codec writes. The "Count" tag belongs to
-- Pawl.Codec.Quantity's Count arm -- this codec's only caller -- so the two
-- types never share one tag across two levels.
--
-- Parametric for 'Pawl.Codec.Aggregation''s reason, and converts with it.
codec :: (Typeable.Typeable q) => Codec.Codec q -> Codec.Codec (Count.Count q)
codec quantityCodec =
  Fields.object $
    Count.MkCount
      <$> Fields.required "scope" Scope.codec Count.scope
      <*> Fields.required "filter" (Filter.codec Keyword.codec) Count.filter
      <*> Fields.required "aggregation" (Aggregation.codec quantityCodec) Count.aggregation

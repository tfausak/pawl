module Pawl.Codec.Count where

import qualified Data.Text as Text
import qualified Pawl.Codec.Aggregation as Aggregation
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Scope as Scope
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Count as Count

-- | A BARE OBJECT keyed by the record's field names, the shape every
-- single-constructor record in this codec writes (Pawl.Codec.Condition,
-- Pawl.Codec.Countering, ...).
--
-- The "Count" TAG MOVED UP to Pawl.Codec.Quantity's Count arm, which is this
-- codec's only caller. It sat here before only because Quantity dispatched on
-- it, which made the two types share one tag across two levels and forced
-- Quantity's arm to skip the wrapper every other arm writes. Quantity now tags
-- its own arm like the rest, and a Count is just the payload.
toJson :: (q -> Value.Value) -> Count.Count q -> Value.Value
toJson codec count =
  Common.object . concat $
    [ Common.requiredPair "scope" Scope.toJson (Count.scope count),
      Common.requiredPair "filter" (Filter.toJson Keyword.toJson) (Count.filter count),
      Common.requiredPair "aggregation" (Aggregation.toJson codec) (Count.aggregation count)
    ]

fromJson :: (Value.Value -> Either Text.Text q) -> Value.Value -> Either Text.Text (Count.Count q)
fromJson decode value = do
  ps <- Common.asObject value
  s <- Common.field "scope" ps >>= Scope.fromJson
  f <- Common.field "filter" ps >>= Filter.fromJson Keyword.fromJson
  a <- Common.field "aggregation" ps >>= Aggregation.fromJson decode
  pure
    Count.MkCount
      { Count.scope = s,
        Count.filter = f,
        Count.aggregation = a
      }

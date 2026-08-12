module Pawl.Codec.Count where

import qualified Data.Text as Text
import qualified Pawl.Codec.Aggregation as Aggregation
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Scope as Scope
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Count as Count

-- | A BARE OBJECT keyed by the record's field names, the shape every
-- single-constructor record in this codec writes. The "Count" tag belongs to
-- Pawl.Codec.Quantity's Count arm -- this codec's only caller -- so the two
-- types never share one tag across two levels.
toJson :: (q -> Value.Value) -> Count.Count q -> Value.Value
toJson codec count =
  Value.object . concat $
    [ Common.requiredPair "scope" Scope.toJson (Count.scope count),
      Common.requiredPair "filter" (Codec.encode (Filter.codec Keyword.codec)) (Count.filter count),
      Common.requiredPair "aggregation" (Aggregation.toJson codec) (Count.aggregation count)
    ]

fromJson :: (Value.Value -> Either Text.Text q) -> Value.Value -> Either Text.Text (Count.Count q)
fromJson decode value = do
  ps <- Common.asObject value
  s <- Common.field "scope" ps >>= Scope.fromJson
  f <- Common.field "filter" ps >>= Codec.decode (Filter.codec Keyword.codec)
  a <- Common.field "aggregation" ps >>= Aggregation.fromJson decode
  pure
    Count.MkCount
      { Count.scope = s,
        Count.filter = f,
        Count.aggregation = a
      }

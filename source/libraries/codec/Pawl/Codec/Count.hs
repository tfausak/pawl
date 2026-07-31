-- | The @Count ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.Count where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.Aggregation (aggregationToJson, jsonToAggregation)
import Pawl.Codec.Filter (filterToJson, jsonToFilter)
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.Scope (jsonToScope, scopeToJson)
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array))
import qualified Pawl.Types.Count as Count.Type

countToJson :: (q -> Value) -> Count.Type.Count q -> Value
countToJson codec (Count.Type.MkCount s f a) =
  Json.tagged (Text.pack "Count") (Just (Array (MkArray [scopeToJson s, filterToJson f, aggregationToJson codec a])))

jsonToCount :: (Value -> Either Text q) -> Value -> Either Text (Count.Type.Count q)
jsonToCount decode value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Count", Just (Array (MkArray [s, f, a]))) -> Count.Type.MkCount <$> jsonToScope s <*> jsonToFilter f <*> jsonToAggregation decode a
    _ -> Left (Text.pack "unknown Count: " <> t)

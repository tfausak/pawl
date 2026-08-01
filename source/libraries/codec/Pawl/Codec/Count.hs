-- | The @Count ⇆ Json@ codec (#481).
module Pawl.Codec.Count where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Aggregation as Aggregation
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.Scope (jsonToScope, scopeToJson)
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array))
import qualified Pawl.Types.Count as Count.Type

countToJson :: (q -> Value) -> Count.Type.Count q -> Value
countToJson codec (Count.Type.MkCount s f a) =
  Json.tagged (Text.pack "Count") (Just (Array (MkArray [scopeToJson s, Filter.toJson f, Aggregation.toJson codec a])))

jsonToCount :: (Value -> Either Text q) -> Value -> Either Text (Count.Type.Count q)
jsonToCount decode value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Count", Just (Array (MkArray [s, f, a]))) -> Count.Type.MkCount <$> jsonToScope s <*> Filter.fromJson f <*> Aggregation.fromJson decode a
    _ -> Left (Text.pack "unknown Count: " <> t)

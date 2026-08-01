module Pawl.Codec.Count where

import qualified Data.Text as Text
import qualified Pawl.Codec.Aggregation as Aggregation
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Scope as Scope
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Count as Count

toJson :: (q -> Value.Value) -> Count.Count q -> Value.Value
toJson codec (Count.MkCount s f a) =
  Common.tagged "Count" . Just . Common.array $ [Scope.toJson s, Filter.toJson f, Aggregation.toJson codec a]

fromJson :: (Value.Value -> Either Text.Text q) -> Value.Value -> Either Text.Text (Count.Count q)
fromJson decode value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("Count", Just (Value.Array (Array.MkArray [s, f, a]))) -> Count.MkCount <$> Scope.fromJson s <*> Filter.fromJson f <*> Aggregation.fromJson decode a
    _ -> Left . Text.pack $ "unknown Count: " <> t

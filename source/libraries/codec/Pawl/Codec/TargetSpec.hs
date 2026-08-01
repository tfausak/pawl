module Pawl.Codec.TargetSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Pool as Pool
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TargetSpec as TargetSpec

-- The product shape: {"pool": <pool>, "filter": <filter | omitted>}. The filter
-- key is omitted when Nothing (a bare "target creature" narrows nothing),
-- mirroring how optional fields are encoded elsewhere. CR 601.2c's "another" is
-- a Not IsSource conjunct inside that filter, not a key of its own (#163).
toJson :: TargetSpec.TargetSpec -> Value.Value
toJson (TargetSpec.MkTargetSpec pool restriction) =
  Common.object $
    Common.pair "pool" (Pool.toJson pool) : case restriction of
      Nothing -> []
      Just f -> [Common.pair "filter" (Filter.toJson f)]

fromJson :: Value.Value -> Either Text.Text TargetSpec.TargetSpec
fromJson value = do
  ps <- Common.asObject value
  pool <- Common.field "pool" ps >>= Pool.fromJson
  restriction <- case Common.optionalField "filter" ps of
    Nothing -> Right Nothing
    Just v -> Just <$> Filter.fromJson v
  pure (TargetSpec.MkTargetSpec pool restriction)

-- A name-keyed map as a sorted array of entries, so the render is deterministic
-- and the file byte-stable.
toJsonMap :: Map.Map SlotName.SlotName TargetSpec.TargetSpec -> Value.Value
toJsonMap m =
  Common.encodeList
    (\(k, v) -> Common.object [Common.pair "slot" (SlotName.toJson k), Common.pair "spec" (toJson v)])
    (Map.toAscList m)

fromJsonMap :: Value.Value -> Either Text.Text (Map.Map SlotName.SlotName TargetSpec.TargetSpec)
fromJsonMap value =
  let decodeEntry v = do
        ps <- Common.asObject v
        k <- Common.field "slot" ps >>= SlotName.fromJson
        spec <- Common.field "spec" ps >>= fromJson
        pure (k, spec)
   in Map.fromList <$> Common.decodeList decodeEntry value

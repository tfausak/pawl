module Pawl.Codec.TargetSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Pool as Pool
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Codec.TargetCount as TargetCount
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TargetCount as TargetCount
import qualified Pawl.Types.TargetSpec as TargetSpec

-- The filter key is omitted when Nothing, mirroring how optional fields are
-- encoded elsewhere. CR 601.2c's "another" is a Not IsSource conjunct inside
-- that filter, not a key of its own (#163).
--
-- The count key is omitted when the slot takes exactly one, so an ordinary
-- "target creature" renders as it always did and only CR 601.2c's variable
-- counts spend a key.
toJson :: TargetSpec.TargetSpec -> Value.Value
toJson spec =
  Value.object $
    Common.requiredPair "pool" Pool.toJson (TargetSpec.pool spec)
      <> Common.optionalPair "filter" Nothing (Common.encodeMaybe (Codec.encode (Filter.codec Keyword.codec))) (TargetSpec.filter spec)
      <> Common.optionalPair "count" TargetCount.one TargetCount.toJson (TargetSpec.count spec)

fromJson :: Value.Value -> Either Text.Text TargetSpec.TargetSpec
fromJson value = do
  ps <- Common.asObject value
  p <- Common.field "pool" ps >>= Pool.fromJson
  f <- Common.defaultedField "filter" Nothing (Common.decodeMaybe (Codec.decode (Filter.codec Keyword.codec))) ps
  c <- Common.defaultedField "count" TargetCount.one TargetCount.fromJson ps
  pure
    TargetSpec.MkTargetSpec
      { TargetSpec.pool = p,
        TargetSpec.filter = f,
        TargetSpec.count = c
      }

-- A name-keyed map as a sorted array of entries, so the render is
-- deterministic.
toJsonMap :: Map.Map SlotName.SlotName TargetSpec.TargetSpec -> Value.Value
toJsonMap m =
  Common.encodeList
    (\(k, v) -> Value.object [Value.pair "slot" (SlotName.toJson k), Value.pair "spec" (toJson v)])
    (Map.toAscList m)

fromJsonMap :: Value.Value -> Either Text.Text (Map.Map SlotName.SlotName TargetSpec.TargetSpec)
fromJsonMap value =
  let decodeEntry v = do
        ps <- Common.asObject v
        k <- Common.field "slot" ps >>= SlotName.fromJson
        spec <- Common.field "spec" ps >>= fromJson
        pure (k, spec)
   in Map.fromList <$> Common.decodeList decodeEntry value

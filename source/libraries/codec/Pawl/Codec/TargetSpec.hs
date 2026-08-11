module Pawl.Codec.TargetSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Pool as Pool
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Codec.TargetRequirement as TargetRequirement
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TargetRequirement as TargetRequirement
import qualified Pawl.Types.TargetSpec as TargetSpec

-- The filter key is omitted when Nothing, mirroring how optional fields are
-- encoded elsewhere. CR 601.2c's "another" is a Not IsSource conjunct inside
-- that filter, not a key of its own (#163).
--
-- The requirement key is omitted when Required, so an ordinary "target creature"
-- renders as it always did and only CR 115.6's "up to one" spends a key.
toJson :: TargetSpec.TargetSpec -> Value.Value
toJson spec =
  Common.object $
    Common.requiredPair "pool" Pool.toJson (TargetSpec.pool spec)
      <> Common.optionalPair "filter" Nothing (Common.encodeMaybe (Filter.toJson Keyword.toJson)) (TargetSpec.filter spec)
      <> Common.optionalPair "requirement" TargetRequirement.Required TargetRequirement.toJson (TargetSpec.requirement spec)

fromJson :: Value.Value -> Either Text.Text TargetSpec.TargetSpec
fromJson value = do
  ps <- Common.asObject value
  p <- Common.field "pool" ps >>= Pool.fromJson
  f <- Common.defaultedField "filter" Nothing (Common.decodeMaybe (Filter.fromJson Keyword.fromJson)) ps
  r <- Common.defaultedField "requirement" TargetRequirement.Required TargetRequirement.fromJson ps
  pure
    TargetSpec.MkTargetSpec
      { TargetSpec.pool = p,
        TargetSpec.filter = f,
        TargetSpec.requirement = r
      }

-- A name-keyed map as a sorted array of entries, so the render is
-- deterministic.
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

-- | The @TargetSpec ⇆ Json@ codec (#481).
module Pawl.Codec.TargetSpec where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.Filter (filterToJson, jsonToFilter)
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.Keyword (jsonToKeyword, keywordToJson)
import Pawl.Codec.Pool (jsonToPool, poolToJson)
import Pawl.Codec.SlotName (jsonToSlotName, slotNameToJson)
import Pawl.Json.Value (Value)
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TargetSpec as TargetSpec

-- The product shape: {"pool": <pool>, "filter": <filter | omitted>}. The filter
-- key is omitted when Nothing (a bare "target creature" narrows nothing),
-- mirroring how optional fields are encoded elsewhere. CR 601.2c's "another" is
-- a Not IsSource conjunct inside that filter, not a key of its own (#163).
targetSpecToJson :: TargetSpec.TargetSpec -> Value
targetSpecToJson (TargetSpec.MkTargetSpec pool restriction) =
  let base = [(Text.pack "pool", poolToJson pool)]
      withFilter = case restriction of
        Nothing -> base
        Just f -> base <> [(Text.pack "filter", filterToJson keywordToJson f)]
   in Json.jObject withFilter

jsonToTargetSpec :: Value -> Either Text TargetSpec.TargetSpec
jsonToTargetSpec value = do
  ps <- Json.asObject value
  pool <- Json.field (Text.pack "pool") ps >>= jsonToPool
  restriction <- case Json.optField (Text.pack "filter") ps of
    Nothing -> Right Nothing
    Just v -> Just <$> jsonToFilter jsonToKeyword v
  pure (TargetSpec.MkTargetSpec pool restriction)

targetSpecsToJson :: Map.Map SlotName.SlotName TargetSpec.TargetSpec -> Value
targetSpecsToJson m =
  Json.listTo (\(k, v) -> Json.jObject [(Text.pack "slot", slotNameToJson k), (Text.pack "spec", targetSpecToJson v)]) (Map.toAscList m)

jsonToTargetSpecs :: Value -> Either Text (Map.Map SlotName.SlotName TargetSpec.TargetSpec)
jsonToTargetSpecs value =
  let decodeEntry v = do
        ps <- Json.asObject v
        k <- Json.field (Text.pack "slot") ps >>= jsonToSlotName
        s <- Json.field (Text.pack "spec") ps >>= jsonToTargetSpec
        pure (k, s)
   in Map.fromList <$> Json.listFrom decodeEntry value

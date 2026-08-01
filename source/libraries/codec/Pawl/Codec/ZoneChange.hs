-- | The @ZoneChange ⇆ Json@ codec (#481).
module Pawl.Codec.ZoneChange where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import qualified Pawl.Codec.ObjectId as ObjectId
import Pawl.Codec.Zone (jsonToZone, zoneToJson)
import Pawl.Json.Value (Value)
import qualified Pawl.Types.ZoneChange as ZoneChange

zoneChangeToJson :: ZoneChange.ZoneChange -> Value
zoneChangeToJson zc =
  Json.jObject
    [ (Text.pack "departed", ObjectId.toJson (ZoneChange.departed zc)),
      (Text.pack "object", ObjectId.toJson (ZoneChange.object zc)),
      (Text.pack "from", zoneToJson (ZoneChange.from zc)),
      (Text.pack "to", zoneToJson (ZoneChange.to zc))
    ]

jsonToZoneChange :: Value -> Either Text ZoneChange.ZoneChange
jsonToZoneChange value = do
  ps <- Json.asObject value
  d <- Json.field (Text.pack "departed") ps >>= ObjectId.fromJson
  o <- Json.field (Text.pack "object") ps >>= ObjectId.fromJson
  f <- Json.field (Text.pack "from") ps >>= jsonToZone
  t <- Json.field (Text.pack "to") ps >>= jsonToZone
  pure (ZoneChange.MkZoneChange d o f t)

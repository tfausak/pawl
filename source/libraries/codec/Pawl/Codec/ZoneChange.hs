module Pawl.Codec.ZoneChange where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.ZoneChange as ZoneChange

toJson :: ZoneChange.ZoneChange -> Value.Value
toJson zc =
  Common.object . concat $
    [ Common.requiredPair "departed" ObjectId.toJson (ZoneChange.departed zc),
      Common.requiredPair "object" ObjectId.toJson (ZoneChange.object zc),
      Common.requiredPair "from" Zone.toJson (ZoneChange.from zc),
      Common.requiredPair "to" Zone.toJson (ZoneChange.to zc)
    ]

fromJson :: Value.Value -> Either Text.Text ZoneChange.ZoneChange
fromJson value = do
  ps <- Common.asObject value
  d <- Common.field "departed" ps >>= ObjectId.fromJson
  o <- Common.field "object" ps >>= ObjectId.fromJson
  f <- Common.field "from" ps >>= Zone.fromJson
  t <- Common.field "to" ps >>= Zone.fromJson
  pure (ZoneChange.MkZoneChange d o f t)

module Pawl.Codec.ZoneChange where

import qualified Data.Text as Text
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ZoneChange as ZoneChange

toJson :: ZoneChange.ZoneChange -> Value.Value
toJson zc =
  Value.object . concat $
    [ Common.requiredPair "departed" (Codec.encode ObjectId.codec) (ZoneChange.departed zc),
      Common.requiredPair "object" (Codec.encode ObjectId.codec) (ZoneChange.object zc),
      Common.requiredPair "from" (Codec.encode Zone.codec) (ZoneChange.from zc),
      Common.requiredPair "to" (Codec.encode Zone.codec) (ZoneChange.to zc)
    ]

fromJson :: Value.Value -> Either Text.Text ZoneChange.ZoneChange
fromJson value = do
  ps <- Common.asObject value
  d <- Common.field "departed" ps >>= Codec.decode ObjectId.codec
  o <- Common.field "object" ps >>= Codec.decode ObjectId.codec
  f <- Common.field "from" ps >>= Codec.decode Zone.codec
  t <- Common.field "to" ps >>= Codec.decode Zone.codec
  pure (ZoneChange.MkZoneChange d o f t)

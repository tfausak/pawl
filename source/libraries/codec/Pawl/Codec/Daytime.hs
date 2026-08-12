module Pawl.Codec.Daytime where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Daytime as Daytime

toJson :: Daytime.Daytime -> Value.Value
toJson d = Common.nullary $ case d of
  Daytime.Day -> "Day"
  Daytime.Night -> "Night"

fromJson :: Value.Value -> Either Text.Text Daytime.Daytime
fromJson =
  Common.decodeNullary
    "Daytime"
    [ ("Day", Daytime.Day),
      ("Night", Daytime.Night)
    ]

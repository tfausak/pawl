module Pawl.Codec.Power where

import qualified Data.Text as Text
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Power as Power

toJson :: Power.Power -> Value.Value
toJson = Codec.encode Quantity.codec . Power.unwrap

fromJson :: Value.Value -> Either Text.Text Power.Power
fromJson = fmap Power.MkPower . Codec.decode Quantity.codec

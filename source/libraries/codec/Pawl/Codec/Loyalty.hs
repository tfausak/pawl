module Pawl.Codec.Loyalty where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Loyalty as Loyalty

toJson :: Loyalty.Loyalty -> Value.Value
toJson = Common.encodeNatural . Loyalty.unwrap

fromJson :: Value.Value -> Either Text.Text Loyalty.Loyalty
fromJson = fmap Loyalty.MkLoyalty . Common.decodeNatural

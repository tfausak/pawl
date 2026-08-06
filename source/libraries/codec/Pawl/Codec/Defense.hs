module Pawl.Codec.Defense where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Defense as Defense

toJson :: Defense.Defense -> Value.Value
toJson = Common.encodeNatural . Defense.unwrap

fromJson :: Value.Value -> Either Text.Text Defense.Defense
fromJson = fmap Defense.MkDefense . Common.decodeNatural

module Pawl.Codec.AbilityName where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.AbilityName as AbilityName

fromJson :: Value.Value -> Either Text.Text AbilityName.AbilityName
fromJson = fmap AbilityName.MkAbilityName . Common.asText

toJson :: AbilityName.AbilityName -> Value.Value
toJson = Common.text . AbilityName.unwrap

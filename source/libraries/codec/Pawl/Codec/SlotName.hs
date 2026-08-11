module Pawl.Codec.SlotName where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.SlotName as SlotName

toJson :: SlotName.SlotName -> Value.Value
toJson = Value.text . SlotName.unwrap

fromJson :: Value.Value -> Either Text.Text SlotName.SlotName
fromJson = fmap SlotName.MkSlotName . Common.asText

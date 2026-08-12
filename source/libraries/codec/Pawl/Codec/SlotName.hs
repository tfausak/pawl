module Pawl.Codec.SlotName where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.SlotName as SlotName

-- | The bundled form of the pair below, for an arm that needs a
-- 'Codec.Codec' rather than two functions (Pawl.Codec.Filter's
-- @ControlledByBound@). Named in @$defs@ through 'Common.wrapper', a slot name
-- being a domain type a card author sees rather than a structural wrapper.
codec :: Codec.Codec SlotName.SlotName
codec = Common.wrapper Common.text SlotName.MkSlotName SlotName.unwrap

toJson :: SlotName.SlotName -> Value.Value
toJson = Value.text . SlotName.unwrap

fromJson :: Value.Value -> Either Text.Text SlotName.SlotName
fromJson = fmap SlotName.MkSlotName . Common.asText

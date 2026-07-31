-- | The @SlotName ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.SlotName where

import Data.Text (Text)
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.SlotName as SlotName

slotNameToJson :: SlotName.SlotName -> Value
slotNameToJson (SlotName.MkSlotName t) = Json.jText t

jsonToSlotName :: Value -> Either Text SlotName.SlotName
jsonToSlotName value = SlotName.MkSlotName <$> Json.asText value

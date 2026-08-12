module Pawl.Codec.SlotName where

import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.SlotName as SlotName

-- | Named in @$defs@ through 'Common.wrapper', a slot name being a domain type
-- a card author sees rather than a structural wrapper.
codec :: Codec.Codec SlotName.SlotName
codec = Common.wrapper Common.text SlotName.MkSlotName SlotName.unwrap

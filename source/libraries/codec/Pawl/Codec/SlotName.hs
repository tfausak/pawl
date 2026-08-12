module Pawl.Codec.SlotName where

import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.SlotName as SlotName

codec :: Codec.Codec SlotName.SlotName
codec = Common.wrapper Common.text SlotName.MkSlotName SlotName.unwrap

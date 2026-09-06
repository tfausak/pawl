module Pawl.Codec.ZoneScope where

import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ZoneScope as ZoneScope

-- | Tagged, in the shape Pawl.Codec.PlayerRef already uses for the same
-- readings: a nested PlayerScope, or a slot name. The two slot-named arms take
-- the same payload and are told apart by the tag alone, as PlayerRef's InSlot
-- and ControllerOfBound are. The wire format is unchanged by the conversion to a
-- bundle; what it adds is the schema.
codec :: Codec.Codec ZoneScope.ZoneScope
codec =
  Arm.tagged
    [ Arm.payload "Scoped" PlayerScope.codec ZoneScope.Scoped (\x -> case x of ZoneScope.Scoped y -> Just y; _ -> Nothing),
      Arm.payload "InSlot" SlotName.codec ZoneScope.InSlot (\x -> case x of ZoneScope.InSlot y -> Just y; _ -> Nothing),
      Arm.payload "ControllerOfBound" SlotName.codec ZoneScope.ControllerOfBound (\x -> case x of ZoneScope.ControllerOfBound y -> Just y; _ -> Nothing)
    ]

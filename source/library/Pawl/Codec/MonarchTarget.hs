module Pawl.Codec.MonarchTarget where

import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.MonarchTarget as MonarchTarget

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec MonarchTarget.MonarchTarget
codec =
  Arm.tagged
    [ Arm.nullary "TheController" MonarchTarget.TheController,
      Arm.nullary "ControllerOfSource" MonarchTarget.ControllerOfSource,
      Arm.payload "InSlot" SlotName.codec MonarchTarget.InSlot (\x -> case x of MonarchTarget.InSlot y -> Just y; _ -> Nothing)
    ]

module Pawl.Codec.SlotCount where

import qualified Pawl.Codec.TargetCount as TargetCount
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.SlotCount as SlotCount

-- | Tagged, so that CR 601.2c's two readings of a slot's count are told apart by
-- the same @type@ key every other union in a card is. The printed range keeps
-- Pawl.Codec.TargetCount's own object as its payload.
codec :: Codec.Codec SlotCount.SlotCount
codec =
  Arm.tagged
    [ Arm.payload "Printed" TargetCount.codec SlotCount.Printed (\x -> case x of SlotCount.Printed y -> Just y; SlotCount.AnnouncedX -> Nothing),
      Arm.nullary "AnnouncedX" SlotCount.AnnouncedX
    ]

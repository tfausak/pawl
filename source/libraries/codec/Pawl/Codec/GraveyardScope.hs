module Pawl.Codec.GraveyardScope where

import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.GraveyardScope as GraveyardScope

-- | Tagged, in the shape Pawl.Codec.PlayerRef already uses for the same pair of
-- readings: a nested PlayerScope, or a slot name. The wire format is unchanged
-- by the conversion to a bundle; what it adds is the schema.
codec :: Codec.Codec GraveyardScope.GraveyardScope
codec =
  Arm.tagged
    encode
    [ Arm.payload "Scoped" PlayerScope.codec GraveyardScope.Scoped,
      Arm.payload "InSlot" SlotName.codec GraveyardScope.InSlot
    ]
  where
    encode s = case s of
      GraveyardScope.Scoped scope -> Common.tagged "Scoped" . Just $ Codec.encode PlayerScope.codec scope
      GraveyardScope.InSlot n -> Common.tagged "InSlot" . Just $ Codec.encode SlotName.codec n

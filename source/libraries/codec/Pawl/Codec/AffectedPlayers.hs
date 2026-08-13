module Pawl.Codec.AffectedPlayers where

import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.AffectedPlayers as AffectedPlayers
import qualified Pawl.Types.SlotName as SlotName.Type

-- | Tagged, in Pawl.Codec.GraveyardScope's shape for the same pair of readings: a
-- nested PlayerScope, or a slot name.
--
-- The SLOT instantiation only. Pawl.Types.AffectedPlayers is also instantiated at
-- a PlayerId, in Pawl.Types.ActivePlayerEffect, and that one has no codec at all
-- -- a stored value never reaches a card file, which is the whole reason the type
-- is parametric rather than carrying both arms at once. So this is a monomorphic
-- bundle rather than a parametric one: there is no second instantiation to serve.
codec :: Codec.Codec (AffectedPlayers.AffectedPlayers SlotName.Type.SlotName)
codec =
  Arm.tagged
    encode
    [ Arm.payload "Scoped" PlayerScope.codec AffectedPlayers.Scoped,
      Arm.payload "Named" SlotName.codec AffectedPlayers.Named
    ]
  where
    encode affected = case affected of
      AffectedPlayers.Scoped scope -> Common.tagged "Scoped" . Just $ Codec.encode PlayerScope.codec scope
      AffectedPlayers.Named slot -> Common.tagged "Named" . Just $ Codec.encode SlotName.codec slot

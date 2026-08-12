module Pawl.Codec.AffectedPlayers where

import qualified Data.Text as Text
import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Json.Value as Value
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
-- is parametric rather than carrying both arms at once.
toJson :: AffectedPlayers.AffectedPlayers SlotName.Type.SlotName -> Value.Value
toJson affected = case affected of
  AffectedPlayers.Scoped scope -> Common.tagged "Scoped" . Just $ Codec.encode PlayerScope.codec scope
  AffectedPlayers.Named slot -> Common.tagged "Named" . Just $ Codec.encode SlotName.codec slot

fromJson :: Value.Value -> Either Text.Text (AffectedPlayers.AffectedPlayers SlotName.Type.SlotName)
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    -- Common.withValue rather than a `Just v` pattern with a fallthrough, for
    -- GraveyardScope's reason: both arms carry a payload, so a missing one is a
    -- malformed known constructor rather than an unknown one.
    ("Scoped", _) -> Common.withValue mv (fmap AffectedPlayers.Scoped . Codec.decode PlayerScope.codec)
    ("Named", _) -> Common.withValue mv (fmap AffectedPlayers.Named . Codec.decode SlotName.codec)
    _ -> Left . Text.pack $ "unknown AffectedPlayers: " <> t

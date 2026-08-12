module Pawl.Codec.GraveyardScope where

import qualified Data.Text as Text
import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.GraveyardScope as GraveyardScope

-- | Tagged, in the shape Pawl.Codec.PlayerRef already uses for the same pair of
-- readings: a nested PlayerScope, or a slot name.
toJson :: GraveyardScope.GraveyardScope -> Value.Value
toJson s = case s of
  GraveyardScope.Scoped scope -> Common.tagged "Scoped" . Just $ PlayerScope.toJson scope
  GraveyardScope.InSlot n -> Common.tagged "InSlot" . Just $ SlotName.toJson n

fromJson :: Value.Value -> Either Text.Text GraveyardScope.GraveyardScope
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    -- Common.withValue rather than a `Just v` pattern with a fallthrough: both
    -- arms carry a payload, so a missing one is a malformed known constructor
    -- and a fallthrough would report it as an unknown one.
    ("Scoped", _) -> Common.withValue mv (fmap GraveyardScope.Scoped . PlayerScope.fromJson)
    ("InSlot", _) -> Common.withValue mv (fmap GraveyardScope.InSlot . SlotName.fromJson)
    _ -> Left . Text.pack $ "unknown GraveyardScope: " <> t

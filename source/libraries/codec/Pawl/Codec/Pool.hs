module Pawl.Codec.Pool where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Pool as Pool

-- | Tagged rather than nullary-only, because CR 400.1's per-player graveyard makes
-- one arm carry a payload: the PlayerScope saying whose. The nullary arms keep
-- emitting exactly what they did, since Common.nullary IS Common.tagged with no
-- value, so no committed card file changes shape.
--
-- The payload widened from a PlayerRelation to a PlayerScope and no committed
-- file moved either: both types spell their shared arm "You", which is the only
-- one any card had written.
toJson :: Pool.Pool -> Value.Value
toJson p = case p of
  Pool.Creatures -> Common.nullary "Creatures"
  Pool.Players -> Common.nullary "Players"
  Pool.AnyTarget -> Common.nullary "AnyTarget"
  Pool.Permanents -> Common.nullary "Permanents"
  Pool.Spells -> Common.nullary "Spells"
  Pool.Abilities -> Common.nullary "Abilities"
  Pool.SpellsAndPermanents -> Common.nullary "SpellsAndPermanents"
  Pool.CardsInGraveyard scope -> Common.tagged "CardsInGraveyard" . Just $ PlayerScope.toJson scope
  -- Nullary, and that is CR 400.1's "the other zones are shared by all players"
  -- showing through the wire format: exile has no per-player copy for a payload
  -- to select among. See Pawl.Types.Pool.CardsInExile.
  Pool.CardsInExile -> Common.nullary "CardsInExile"

fromJson :: Value.Value -> Either Text.Text Pool.Pool
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("Creatures", _) -> Right Pool.Creatures
    ("Players", _) -> Right Pool.Players
    ("AnyTarget", _) -> Right Pool.AnyTarget
    ("Permanents", _) -> Right Pool.Permanents
    ("Spells", _) -> Right Pool.Spells
    ("Abilities", _) -> Right Pool.Abilities
    ("SpellsAndPermanents", _) -> Right Pool.SpellsAndPermanents
    -- Common.withValue, not a `Just v` pattern with a fallthrough: a
    -- CardsInGraveyard with no value is a MALFORMED known constructor, and the
    -- fallthrough would report it as an unknown one.
    ("CardsInGraveyard", _) -> Common.withValue mv (fmap Pool.CardsInGraveyard . PlayerScope.fromJson)
    ("CardsInExile", _) -> Right Pool.CardsInExile
    _ -> Left . Text.pack $ "unknown Pool: " <> t

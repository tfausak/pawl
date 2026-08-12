module Pawl.Codec.Pool where

import qualified Data.Text as Text
import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Pool as Pool

-- | Tagged rather than nullary-only, because CR 400.1's per-player graveyard
-- makes one arm carry a payload: the PlayerScope saying whose. The nullary arms
-- are unaffected, since Common.nullary IS Common.tagged with no value.
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
  -- Nullary: CR 400.1's shared zones have no per-player copy for a payload to
  -- select among.
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
    -- CardsInGraveyard with no value is a malformed known constructor, and a
    -- fallthrough would report it as an unknown one.
    ("CardsInGraveyard", _) -> Common.withValue mv (fmap Pool.CardsInGraveyard . PlayerScope.fromJson)
    ("CardsInExile", _) -> Right Pool.CardsInExile
    _ -> Left . Text.pack $ "unknown Pool: " <> t

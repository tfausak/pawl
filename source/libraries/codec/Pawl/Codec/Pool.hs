-- | The @Pool ⇆ Json@ codec (#481).
module Pawl.Codec.Pool where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.PlayerScope (jsonToPlayerScope, playerScopeToJson)
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Pool as Pool

-- Tagged rather than nullary-only, because CR 400.1's per-player graveyard makes
-- one arm carry a payload: the PlayerScope saying whose. The nullary arms keep
-- emitting exactly what they did, since Json.nullary IS Json.tagged with no
-- value, so no committed card file changes shape.
--
-- The payload widened from a PlayerRelation to a PlayerScope and no committed
-- file moved either: both types spell their shared arm "You", which is the only
-- one any card had written.
poolToJson :: Pool.Pool -> Value
poolToJson p = case p of
  Pool.Creatures -> Json.nullary (Text.pack "Creatures")
  Pool.Players -> Json.nullary (Text.pack "Players")
  Pool.AnyTarget -> Json.nullary (Text.pack "AnyTarget")
  Pool.Permanents -> Json.nullary (Text.pack "Permanents")
  Pool.Spells -> Json.nullary (Text.pack "Spells")
  Pool.Abilities -> Json.nullary (Text.pack "Abilities")
  Pool.SpellsAndPermanents -> Json.nullary (Text.pack "SpellsAndPermanents")
  Pool.CardsInGraveyard scope -> Json.tagged (Text.pack "CardsInGraveyard") (Just (playerScopeToJson scope))
  -- Nullary, and that is CR 400.1's "the other zones are shared by all players"
  -- showing through the wire format: exile has no per-player copy for a payload
  -- to select among. See Pawl.Types.Pool.CardsInExile.
  Pool.CardsInExile -> Json.nullary (Text.pack "CardsInExile")

jsonToPool :: Value -> Either Text Pool.Pool
jsonToPool value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Creatures", _) -> Right Pool.Creatures
    ("Players", _) -> Right Pool.Players
    ("AnyTarget", _) -> Right Pool.AnyTarget
    ("Permanents", _) -> Right Pool.Permanents
    ("Spells", _) -> Right Pool.Spells
    ("Abilities", _) -> Right Pool.Abilities
    ("SpellsAndPermanents", _) -> Right Pool.SpellsAndPermanents
    -- Json.withValue, not a `Just v` pattern with a fallthrough: a
    -- CardsInGraveyard with no value is a MALFORMED known constructor, and the
    -- fallthrough would report it as an unknown one.
    ("CardsInGraveyard", _) -> Json.withValue mv (fmap Pool.CardsInGraveyard . jsonToPlayerScope)
    ("CardsInExile", _) -> Right Pool.CardsInExile
    _ -> Left (Text.pack "unknown Pool: " <> t)

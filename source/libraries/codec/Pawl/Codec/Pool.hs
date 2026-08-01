-- | The @Pool ⇆ Json@ codec (#481).
module Pawl.Codec.Pool where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.PlayerRelation (jsonToPlayerRelation, playerRelationToJson)
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Pool as Pool

-- Tagged rather than nullary-only, because CR 400.1's per-player graveyard makes
-- one arm carry a payload: the PlayerRelation saying whose. The nullary arms keep
-- emitting exactly what they did, since Json.nullary IS Json.tagged with no
-- value, so no committed card file changes shape.
poolToJson :: Pool.Pool -> Value
poolToJson p = case p of
  Pool.Creatures -> Json.nullary (Text.pack "Creatures")
  Pool.Players -> Json.nullary (Text.pack "Players")
  Pool.AnyTarget -> Json.nullary (Text.pack "AnyTarget")
  Pool.Permanents -> Json.nullary (Text.pack "Permanents")
  Pool.Spells -> Json.nullary (Text.pack "Spells")
  Pool.Abilities -> Json.nullary (Text.pack "Abilities")
  Pool.SpellsAndPermanents -> Json.nullary (Text.pack "SpellsAndPermanents")
  Pool.CardsInGraveyard r -> Json.tagged (Text.pack "CardsInGraveyard") (Just (playerRelationToJson r))

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
    ("CardsInGraveyard", _) -> Json.withValue mv (fmap Pool.CardsInGraveyard . jsonToPlayerRelation)
    _ -> Left (Text.pack "unknown Pool: " <> t)

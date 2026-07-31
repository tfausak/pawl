-- | The @PlayerEffect ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.PlayerEffect where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.Filter (filterToJson, jsonToFilter)
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.ManaCost (jsonToManaCost, manaCostToJson)
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array))
import qualified Pawl.Types.PlayerEffect as PlayerEffect

playerEffectToJson :: PlayerEffect.PlayerEffect -> Value
playerEffectToJson e = case e of
  PlayerEffect.CantCastSpells -> Json.nullary (Text.pack "CantCastSpells")
  PlayerEffect.CantCastMoreThan n -> Json.tagged (Text.pack "CantCastMoreThan") (Just (Json.natTo n))
  PlayerEffect.IncreaseSpellCost c n -> Json.tagged (Text.pack "IncreaseSpellCost") (Just (Array (MkArray [filterToJson c, Json.natTo n])))
  PlayerEffect.ReduceSpellCost c m -> Json.tagged (Text.pack "ReduceSpellCost") (Just (Array (MkArray [filterToJson c, manaCostToJson m])))
  PlayerEffect.NoMaximumHandSize -> Json.nullary (Text.pack "NoMaximumHandSize")
  PlayerEffect.DontLoseUnspentMana -> Json.nullary (Text.pack "DontLoseUnspentMana")

jsonToPlayerEffect :: Value -> Either Text PlayerEffect.PlayerEffect
jsonToPlayerEffect value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("CantCastSpells", _) -> Right PlayerEffect.CantCastSpells
    ("CantCastMoreThan", Just v) -> PlayerEffect.CantCastMoreThan <$> Json.natFrom v
    ("IncreaseSpellCost", Just (Array (MkArray [c, n]))) -> PlayerEffect.IncreaseSpellCost <$> jsonToFilter c <*> Json.natFrom n
    ("ReduceSpellCost", Just (Array (MkArray [c, m]))) -> PlayerEffect.ReduceSpellCost <$> jsonToFilter c <*> jsonToManaCost m
    ("NoMaximumHandSize", _) -> Right PlayerEffect.NoMaximumHandSize
    ("DontLoseUnspentMana", _) -> Right PlayerEffect.DontLoseUnspentMana
    _ -> Left (Text.pack "unknown PlayerEffect: " <> t)

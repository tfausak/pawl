module Pawl.Codec.PlayerEffect where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.Codec.ManaFilter as ManaFilter
import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.PlayerEffect as PlayerEffect

toJson :: PlayerEffect.PlayerEffect -> Value.Value
toJson e = case e of
  PlayerEffect.CantCastSpells -> Common.nullary "CantCastSpells"
  PlayerEffect.CantCastMoreThan n -> Common.tagged "CantCastMoreThan" . Just $ Common.encodeNatural n
  PlayerEffect.IncreaseSpellCost c n -> Common.tagged "IncreaseSpellCost" . Just . Common.array $ [Filter.toJson Keyword.toJson c, Common.encodeNatural n]
  PlayerEffect.ReduceSpellCost c m -> Common.tagged "ReduceSpellCost" . Just . Common.array $ [Filter.toJson Keyword.toJson c, ManaCost.toJson m]
  PlayerEffect.NoMaximumHandSize -> Common.nullary "NoMaximumHandSize"
  PlayerEffect.DontLoseUnspentMana f -> Common.tagged "DontLoseUnspentMana" . Just $ ManaFilter.toJson f
  PlayerEffect.CantBeTargetedBy sc -> Common.tagged "CantBeTargetedBy" . Just $ PlayerScope.toJson sc

fromJson :: Value.Value -> Either Text.Text PlayerEffect.PlayerEffect
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("CantCastSpells", _) -> Right PlayerEffect.CantCastSpells
    ("CantCastMoreThan", Just v) -> PlayerEffect.CantCastMoreThan <$> Common.decodeNatural v
    ("IncreaseSpellCost", Just (Value.Array (Array.MkArray [c, n]))) -> PlayerEffect.IncreaseSpellCost <$> Filter.fromJson Keyword.fromJson c <*> Common.decodeNatural n
    ("ReduceSpellCost", Just (Value.Array (Array.MkArray [c, m]))) -> PlayerEffect.ReduceSpellCost <$> Filter.fromJson Keyword.fromJson c <*> ManaCost.fromJson m
    ("NoMaximumHandSize", _) -> Right PlayerEffect.NoMaximumHandSize
    ("DontLoseUnspentMana", Just v) -> PlayerEffect.DontLoseUnspentMana <$> ManaFilter.fromJson v
    ("CantBeTargetedBy", Just v) -> PlayerEffect.CantBeTargetedBy <$> PlayerScope.fromJson v
    _ -> Left . Text.pack $ "unknown PlayerEffect: " <> t

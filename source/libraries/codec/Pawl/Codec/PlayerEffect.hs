module Pawl.Codec.PlayerEffect where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.DamagePattern as DamagePattern
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
  PlayerEffect.CantCastChosenName -> Common.nullary "CantCastChosenName"
  PlayerEffect.CantPlayLandChosenName -> Common.nullary "CantPlayLandChosenName"
  PlayerEffect.IncreaseSpellCost c n -> Common.tagged "IncreaseSpellCost" . Just . Common.array $ [Filter.toJson Keyword.toJson c, Common.encodeNatural n]
  PlayerEffect.ReduceSpellCost c m -> Common.tagged "ReduceSpellCost" . Just . Common.array $ [Filter.toJson Keyword.toJson c, ManaCost.toJson m]
  PlayerEffect.PlayAdditionalLands n -> Common.tagged "PlayAdditionalLands" . Just $ Common.encodeNatural n
  PlayerEffect.NoMaximumHandSize -> Common.nullary "NoMaximumHandSize"
  PlayerEffect.DontLoseUnspentMana f -> Common.tagged "DontLoseUnspentMana" . Just $ ManaFilter.toJson f
  PlayerEffect.CantBeTargetedBy sc -> Common.tagged "CantBeTargetedBy" . Just $ PlayerScope.toJson sc
  PlayerEffect.CastAsThoughItHadFlash c -> Common.tagged "CastAsThoughItHadFlash" . Just $ Filter.toJson Keyword.toJson c
  PlayerEffect.CantBeCountered c -> Common.tagged "CantBeCountered" . Just $ Filter.toJson Keyword.toJson c
  PlayerEffect.DamageCantBePrevented p -> Common.tagged "DamageCantBePrevented" . Just $ DamagePattern.toJson p
  PlayerEffect.CantSearchLibraries -> Common.nullary "CantSearchLibraries"

fromJson :: Value.Value -> Either Text.Text PlayerEffect.PlayerEffect
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("CantCastSpells", _) -> Right PlayerEffect.CantCastSpells
    ("CantCastMoreThan", Just v) -> PlayerEffect.CantCastMoreThan <$> Common.decodeNatural v
    ("CantCastChosenName", _) -> Right PlayerEffect.CantCastChosenName
    ("CantPlayLandChosenName", _) -> Right PlayerEffect.CantPlayLandChosenName
    ("IncreaseSpellCost", Just (Value.Array (Array.MkArray [c, n]))) -> PlayerEffect.IncreaseSpellCost <$> Filter.fromJson Keyword.fromJson c <*> Common.decodeNatural n
    ("ReduceSpellCost", Just (Value.Array (Array.MkArray [c, m]))) -> PlayerEffect.ReduceSpellCost <$> Filter.fromJson Keyword.fromJson c <*> ManaCost.fromJson m
    ("PlayAdditionalLands", Just v) -> PlayerEffect.PlayAdditionalLands <$> Common.decodeNatural v
    ("NoMaximumHandSize", _) -> Right PlayerEffect.NoMaximumHandSize
    ("DontLoseUnspentMana", Just v) -> PlayerEffect.DontLoseUnspentMana <$> ManaFilter.fromJson v
    ("CantBeTargetedBy", Just v) -> PlayerEffect.CantBeTargetedBy <$> PlayerScope.fromJson v
    ("CastAsThoughItHadFlash", Just v) -> PlayerEffect.CastAsThoughItHadFlash <$> Filter.fromJson Keyword.fromJson v
    ("CantBeCountered", Just v) -> PlayerEffect.CantBeCountered <$> Filter.fromJson Keyword.fromJson v
    ("DamageCantBePrevented", Just v) -> PlayerEffect.DamageCantBePrevented <$> DamagePattern.fromJson v
    ("CantSearchLibraries", _) -> Right PlayerEffect.CantSearchLibraries
    _ -> Left . Text.pack $ "unknown PlayerEffect: " <> t

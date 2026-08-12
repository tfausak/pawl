module Pawl.Codec.PlayerEffect where

import qualified Data.Text as Text
import qualified Pawl.Codec.DamagePattern as DamagePattern
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.Codec.ManaFilter as ManaFilter
import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.PlayerEffect as PlayerEffect

toJson :: PlayerEffect.PlayerEffect -> Value.Value
toJson e = case e of
  PlayerEffect.CantCastSpells -> Common.nullary "CantCastSpells"
  PlayerEffect.CantCastMoreThan n -> Common.tagged "CantCastMoreThan" . Just $ Common.encodeNatural n
  PlayerEffect.CantCastChosenName -> Common.nullary "CantCastChosenName"
  PlayerEffect.CantPlayLandChosenName -> Common.nullary "CantPlayLandChosenName"
  PlayerEffect.IncreaseSpellCost c n -> Common.tagged "IncreaseSpellCost" . Just . Value.array $ [Codec.encode (Filter.codec Keyword.codec) c, Common.encodeNatural n]
  PlayerEffect.ReduceSpellCost c m -> Common.tagged "ReduceSpellCost" . Just . Value.array $ [Codec.encode (Filter.codec Keyword.codec) c, Codec.encode ManaCost.codec m]
  PlayerEffect.ReduceActivationCost c m n -> Common.tagged "ReduceActivationCost" . Just . Value.array $ [Codec.encode (Filter.codec Keyword.codec) c, Codec.encode ManaCost.codec m, Common.encodeNatural n]
  PlayerEffect.PlayAdditionalLands n -> Common.tagged "PlayAdditionalLands" . Just $ Common.encodeNatural n
  PlayerEffect.NoMaximumHandSize -> Common.nullary "NoMaximumHandSize"
  PlayerEffect.SetMaximumHandSize n -> Common.tagged "SetMaximumHandSize" . Just $ Common.encodeNatural n
  PlayerEffect.DontLoseUnspentMana f -> Common.tagged "DontLoseUnspentMana" . Just $ ManaFilter.toJson f
  PlayerEffect.CantBeTargetedBy sc -> Common.tagged "CantBeTargetedBy" . Just $ PlayerScope.toJson sc
  PlayerEffect.CastAsThoughItHadFlash c -> Common.tagged "CastAsThoughItHadFlash" . Just $ Codec.encode (Filter.codec Keyword.codec) c
  PlayerEffect.CantBeCountered c -> Common.tagged "CantBeCountered" . Just $ Codec.encode (Filter.codec Keyword.codec) c
  PlayerEffect.DamageCantBePrevented p -> Common.tagged "DamageCantBePrevented" . Just $ DamagePattern.toJson p
  PlayerEffect.CantSearchLibraries -> Common.nullary "CantSearchLibraries"
  PlayerEffect.CantBecomeMonarch -> Common.nullary "CantBecomeMonarch"
  PlayerEffect.CantCastMatching c -> Common.tagged "CantCastMatching" . Just $ Codec.encode (Filter.codec Keyword.codec) c
  PlayerEffect.CantPlayLands -> Common.nullary "CantPlayLands"

fromJson :: Value.Value -> Either Text.Text PlayerEffect.PlayerEffect
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("CantCastSpells", _) -> Right PlayerEffect.CantCastSpells
    ("CantCastMoreThan", Just v) -> PlayerEffect.CantCastMoreThan <$> Common.decodeNatural v
    ("CantCastChosenName", _) -> Right PlayerEffect.CantCastChosenName
    ("CantPlayLandChosenName", _) -> Right PlayerEffect.CantPlayLandChosenName
    ("IncreaseSpellCost", Just (Value.Array (Array.MkArray [c, n]))) -> PlayerEffect.IncreaseSpellCost <$> Codec.decode (Filter.codec Keyword.codec) c <*> Common.decodeNatural n
    ("ReduceSpellCost", Just (Value.Array (Array.MkArray [c, m]))) -> PlayerEffect.ReduceSpellCost <$> Codec.decode (Filter.codec Keyword.codec) c <*> Codec.decode ManaCost.codec m
    ("ReduceActivationCost", Just (Value.Array (Array.MkArray [c, m, n]))) -> PlayerEffect.ReduceActivationCost <$> Codec.decode (Filter.codec Keyword.codec) c <*> Codec.decode ManaCost.codec m <*> Common.decodeNatural n
    ("PlayAdditionalLands", Just v) -> PlayerEffect.PlayAdditionalLands <$> Common.decodeNatural v
    ("NoMaximumHandSize", _) -> Right PlayerEffect.NoMaximumHandSize
    ("SetMaximumHandSize", Just v) -> PlayerEffect.SetMaximumHandSize <$> Common.decodeNatural v
    ("DontLoseUnspentMana", Just v) -> PlayerEffect.DontLoseUnspentMana <$> ManaFilter.fromJson v
    ("CantBeTargetedBy", Just v) -> PlayerEffect.CantBeTargetedBy <$> PlayerScope.fromJson v
    ("CastAsThoughItHadFlash", Just v) -> PlayerEffect.CastAsThoughItHadFlash <$> Codec.decode (Filter.codec Keyword.codec) v
    ("CantBeCountered", Just v) -> PlayerEffect.CantBeCountered <$> Codec.decode (Filter.codec Keyword.codec) v
    ("DamageCantBePrevented", Just v) -> PlayerEffect.DamageCantBePrevented <$> DamagePattern.fromJson v
    ("CantSearchLibraries", _) -> Right PlayerEffect.CantSearchLibraries
    ("CantBecomeMonarch", _) -> Right PlayerEffect.CantBecomeMonarch
    ("CantCastMatching", Just v) -> PlayerEffect.CantCastMatching <$> Codec.decode (Filter.codec Keyword.codec) v
    ("CantPlayLands", _) -> Right PlayerEffect.CantPlayLands
    _ -> Left . Text.pack $ "unknown PlayerEffect: " <> t

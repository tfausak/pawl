module Pawl.Codec.PlayerEffectSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.PlayerEffect as PlayerEffect
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaFilter as ManaFilter
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.Subtype as Subtype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlayerEffect" $ do
  -- CR 601.3 / Silence.
  Spec.it s "CantCastSpells" $
    Common.assertJsonCodec s PlayerEffect.toJson PlayerEffect.fromJson PlayerEffect.CantCastSpells "{\"type\":\"CantCastSpells\"}"
  -- CR 601.3 / Rule of Law.
  Spec.it s "CantCastMoreThan" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      (PlayerEffect.CantCastMoreThan 1)
      "{\"type\":\"CantCastMoreThan\",\"value\":1}"
  -- CR 613.11 / 601.2f / Thalia.
  Spec.it s "IncreaseSpellCost" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      (PlayerEffect.IncreaseSpellCost (Filter.Not (Filter.HasCardType CardType.Creature)) 1)
      "{\"type\":\"IncreaseSpellCost\",\"value\":[{\"type\":\"Not\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}},1]}"
  -- CR 613.11 / 601.2f / Sapphire Medallion.
  Spec.it s "ReduceSpellCost" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      (PlayerEffect.ReduceSpellCost (Filter.HasColor Color.Blue) (ManaCost.MkManaCost [ManaSymbol.Generic 1]))
      "{\"type\":\"ReduceSpellCost\",\"value\":[{\"type\":\"HasColor\",\"value\":{\"type\":\"Blue\"}},[{\"type\":\"Generic\",\"value\":1}]]}"
  -- Edgewalker's: the reduction that names a mana type, which the Medallion's
  -- generic one above would not catch a regression in.
  Spec.it s "ReduceSpellCost, naming a mana type" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      ( PlayerEffect.ReduceSpellCost
          (Filter.HasSubtype Subtype.Cleric)
          (ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.White), ManaSymbol.OfType (ManaType.Colored Color.Black)])
      )
      "{\"type\":\"ReduceSpellCost\",\"value\":[{\"type\":\"HasSubtype\",\"value\":{\"type\":\"Cleric\"}},[{\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"White\"}}},{\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"Black\"}}}]]}"
  -- CR 402.2 / Reliquary Tower.
  Spec.it s "NoMaximumHandSize" $
    Common.assertJsonCodec s PlayerEffect.toJson PlayerEffect.fromJson PlayerEffect.NoMaximumHandSize "{\"type\":\"NoMaximumHandSize\"}"
  -- CR 500.5 / 703.4q / Upwelling: no mana type named.
  Spec.it s "DontLoseUnspentMana, Upwelling's whole pool" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      (PlayerEffect.DontLoseUnspentMana ManaFilter.Any)
      "{\"type\":\"DontLoseUnspentMana\",\"value\":{\"type\":\"Any\"}}"
  -- CR 106.1a / Omnath, Locus of Mana: the OTHER filter, which is the whole
  -- difference between the two producers -- so a codec that dropped the payload
  -- would round-trip one of these and not both.
  Spec.it s "DontLoseUnspentMana, Omnath's green only" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      (PlayerEffect.DontLoseUnspentMana (ManaFilter.OfType (ManaType.Colored Color.Green)))
      "{\"type\":\"DontLoseUnspentMana\",\"value\":{\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"Green\"}}}}"
  -- CR 702.18a / Ivory Mask: shroud names no player, so the scope is everybody.
  Spec.it s "CantBeTargetedBy, shroud's EachPlayer" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      (PlayerEffect.CantBeTargetedBy PlayerScope.EachPlayer)
      "{\"type\":\"CantBeTargetedBy\",\"value\":{\"type\":\"EachPlayer\"}}"
  -- CR 702.11c / Leyline of Sanctity: the OTHER scope, which is the whole
  -- difference between the two keywords -- so a codec that dropped the payload
  -- would round-trip one of these and not both.
  Spec.it s "CantBeTargetedBy, hexproof's Opponents" $
    Common.assertJsonCodec
      s
      PlayerEffect.toJson
      PlayerEffect.fromJson
      (PlayerEffect.CantBeTargetedBy PlayerScope.Opponents)
      "{\"type\":\"CantBeTargetedBy\",\"value\":{\"type\":\"Opponents\"}}"
